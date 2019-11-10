command! MakeTags !ctags -f .tags -R

set tags+=../../include/PA2/.tags
set path+=../../include/PA2/**

nnoremap <F4> :make<cr>
set makeprg=make\ dotest
