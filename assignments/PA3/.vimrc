set tags+=../../include/PA3/.tags
set path+=../../include/PA3/**

set makeprg=make\ dotest
nnoremap <F4> :AsyncRun make<cr>
nnoremap <F5> :AsyncRun make dotest<cr>
nnoremap <F6> :AsyncRun make parser && make && ./myparser mygood.cl <cr>
nnoremap <F7> :AsyncRun make parser && make && ./myparser mygood.cl -p<cr>
nnoremap <F8> :AsyncRun make parser && make && ./myparser mybad.cl <cr>
