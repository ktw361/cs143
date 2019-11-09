/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <sstream>      // for stringstream

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
bool string_too_long();
int comment_level = 0;  // nested comment level
int cur_str_size = 0;
bool bad_eof = false; // EOF in str or comment

%}

/*
 * Define names for regular expressions here.
 */

DARROW          "=>"
LESS            "<"
LESSEQUAL       "<="
EQUAL           "="
ASSIGN          "<-"
ADD             "+"
DEC             "-"
MUL             "*"
DIV             "/"
DOT             "."
TILDE           "~"
AMPERSAT        "@"

PARENOPEN       \(
PARENCLOSE      \)
BRACEOPEN       \{
BRACECLOSE      \}
SEMICOLON       \;
COLON           \:
COMMA           \,

CAPITAL         [A-Z]
LETTER          [a-z]
ALPHAUNDER      [a-zA-Z_]
VALIDCHAR       [0-9a-zA-Z_]
DIGIT           [0-9]
WHITESPACE      [\ \t\f\r\v]
NEWLINE         \n

SELF            self
SELF_TYPE       SELF_TYPE

aA              a|A
cC              c|C
dD              d|D
eE              e|E
fF              f|F
hH              h|H
iI              i|I
lL              l|L
nN              n|N
wW              w|W
oO              o|O
pP              p|P
rR              r|R
sS              s|S
tT              t|T
uU              u|U
vV              v|V

CLASS           {cC}{lL}{aA}{sS}{sS}
INHERITS        {iI}{nN}{hH}{eE}{rR}{iI}{tT}{sS}

IF              {iI}{fF}
THEN            {tT}{hH}{eE}{nN}
ELSE            {eE}{lL}{sS}{eE}
FI              {fF}{iI}

WHILE           {wW}{hH}{iI}{lL}{eE}
LOOP            {lL}{oO}{oO}{pP}
POOL            {pP}{oO}{oO}{lL}

LET             {lL}{eE}{tT}
IN              {iI}{nN}
CASE            {cC}{aA}{sS}{eE}
ESAC            {eE}{sS}{aA}{cC}
OF              {oO}{fF}

NEW             {nN}{eE}{wW}
NOT             {nN}{oO}{tT}
ISVOID          {iI}{sS}{vV}{oO}{iI}{dD}
TRUE            t{rR}{uU}{eE}
FALSE           f{aA}{lL}{sS}{eE}

COMMENTSTART    "(*"
COMMENTCLOSE    "*)"
COMMENT         "--"
QUOTE           \"

%s              IN_NESTCOMMENT
%s              IN_COMMENT
%s              IN_STRING
%s              STRING_ERR

%%

<INITIAL>{
{COMMENT}         { BEGIN(IN_COMMENT); }
{COMMENTSTART}    {
    BEGIN(IN_NESTCOMMENT);
    comment_level++; }
{COMMENTCLOSE}    {
    cool_yylval.error_msg = "Unmatched *)";
    return (ERROR); }
{QUOTE}             {
    BEGIN(IN_STRING);
    string_buf_ptr = string_buf;
    cur_str_size = 0;
}
}

 /*
  *  Nested comments
  */
<IN_COMMENT>{
{NEWLINE}         {
    BEGIN(INITIAL);
    curr_lineno++; }
<<EOF>>         { return 0; }
.+   { }
}

<IN_NESTCOMMENT>{
{COMMENTSTART}    { comment_level++; }
{COMMENTCLOSE}    {
    comment_level--;
    if (comment_level == 0)
        BEGIN(INITIAL);
                }
{NEWLINE}         { curr_lineno++; }
<<EOF>>         {
    cool_yylval.error_msg = "EOF in comment";
    if (! bad_eof) {
        bad_eof = true;
        return (ERROR);
    } else {
        return 0;
    }
}
[^*\n]+         { 
    // eat anything that is not * or \n
}
"*"             {
    // eat the lone star
}
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
<IN_STRING>{
{QUOTE} {
    BEGIN(INITIAL);
    cool_yylval.symbol = stringtable.add_string(string_buf_ptr, cur_str_size);
    return (STR_CONST);
}
\\{WHITESPACE}*\n {
    // escaped new line
    string_buf_ptr[cur_str_size++] = '\n';
    if (string_too_long()) return (ERROR);
    curr_lineno++;
}
\\[^btnf] {
    string_buf_ptr[cur_str_size++] = yytext[1];
    if (string_too_long()) return (ERROR);
}
\\n { 
    string_buf_ptr[cur_str_size++] = '\n'; 
    if (string_too_long()) return (ERROR);
    }
\\t { 
    string_buf_ptr[cur_str_size++] = '\t'; 
    if (string_too_long()) return (ERROR);
    }
\\b { 
    string_buf_ptr[cur_str_size++] = '\b'; 
    if (string_too_long()) return (ERROR);
    }
\\f { 
    string_buf_ptr[cur_str_size++] = '\f'; 
    if (string_too_long()) return (ERROR);
    }
<<EOF>>                 {
    cool_yylval.error_msg = "EOF in string constant";
    if (! bad_eof) {
        bad_eof = true;
        return (ERROR);
    } else 
        return 0;
}
{NEWLINE} {
    cool_yylval.error_msg = "Unterminated string constant";
    curr_lineno++;
    BEGIN(INITIAL);  // assume programmer forget
    return (ERROR);
}
\0                      {
    cool_yylval.error_msg = "String contains null character";
    BEGIN(STRING_ERR);
    /* return (ERROR); */
}
[^\\"\n]+    {
    int textlen = strlen(yytext);
    if (cur_str_size + textlen > MAX_STR_CONST) {
        cool_yylval.error_msg = "String constant too long";
        BEGIN(STRING_ERR);
        return (ERROR);
    }
    strcpy(string_buf_ptr + cur_str_size, yytext);
    cur_str_size += textlen;
}
}

<STRING_ERR>{
{NEWLINE}   {
    curr_lineno++;
    BEGIN(INITIAL);
}
{QUOTE}     {
    BEGIN(INITIAL);
}
.           {
    // Come on, please end this hell.
}
}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{ASSIGN}        { return (ASSIGN); }
{NEW}           { return (NEW); }
{ISVOID}        { return (ISVOID); }
{LESSEQUAL}     { return (LE); }
{NOT}           { return (NOT); }

 /*
  *  Spaces.
  */
{WHITESPACE}*   { }
{NEWLINE}       { curr_lineno++; }

 /*
  *  Single-character operators.
  */
{LESS}          { return ('<'); }
{EQUAL}         { return ('='); }
{ADD}           { return ('+'); }
{DEC}           { return ('-'); }
{MUL}           { return ('*'); }
{DIV}           { return ('/'); }
{DOT}           { return ('.'); }
{TILDE}         { return ('~'); }
{AMPERSAT}      { return ('@'); }
{PARENOPEN}     { return ('('); }
{PARENCLOSE}    { return (')'); }
{BRACEOPEN}     { return ('{'); }
{BRACECLOSE}    { return ('}'); }
{SEMICOLON}     { return (';'); }
{COLON}         { return (':'); }
{COMMA}         { return (','); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}         { return (CLASS); }
{ELSE}          { return (ELSE); }
{FI}            { return (FI); }
{IF}            { return (IF); }
{THEN}          { return (THEN); }
{IN}            { return {IN}; }
{INHERITS}      { return (INHERITS); }
{LET}           { return (LET); }
{LOOP}          { return (LOOP); }
{POOL}          { return (POOL); }
{WHILE}         { return (WHILE); }
{CASE}          { return (CASE); }
{ESAC}          { return (ESAC); }
{OF}            { return (OF); }

 /*
  * Bool constant has higher matching priority
  */
{TRUE} { 
    cool_yylval.boolean = 1;
    return (BOOL_CONST);
}
{FALSE} {
    cool_yylval.boolean = 0;
    return (BOOL_CONST);
}

 /*
  * TypeIdentifier & ObjectIdentifier
  */
{CAPITAL}{VALIDCHAR}* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return (TYPEID);
}

{LETTER}{VALIDCHAR}* {
    cool_yylval.symbol = idtable.add_string(yytext);
    return (OBJECTID);
}


{DIGIT}+        { 
    std::stringstream ss;
    int num;
    ss << yytext;
    ss >> num;
    cool_yylval.symbol = inttable.add_int(num);
    return (INT_CONST); 
}

{DIGIT}+{ALPHAUNDER}{VALIDCHAR}*        { 
    strcpy(cool_yylval.error_msg, yytext);
    return (ERROR);
}

.           {
    // Any other char is invalid
    strcpy(cool_yylval.error_msg, yytext);
    return (ERROR);
}

<<EOF>> { return 0; }

%%
bool string_too_long() {
    if (cur_str_size >= MAX_STR_CONST) {
        cool_yylval.error_msg = "String constant too long";
        BEGIN(STRING_ERR);
        return true;
    } else
        return false;
}
