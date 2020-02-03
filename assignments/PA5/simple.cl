class Main {
  a : Int <- 2;
  s : String <- "init";
  main() : Object {
{
    a <- 9;
    s <- "next";
    (new IO).out_string(s);
}
  };
};
