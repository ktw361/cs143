set tags+=../../include/PA3/.tags
set path+=../../include/PA3/**

set makeprg=make\ dotest
nnoremap <F4> :AsyncRun make<cr>
nnoremap <F5> :AsyncRun make && ./myparser mygood.cl <cr>
nnoremap <F6> :AsyncRun make dotest<cr>
