set tags+=../../include/PA4/.tags
set path+=../../include/PA4/**

set makeprg=make\ dotest
" nnoremap <F4> :AsyncRun make semant<cr>
" nnoremap <F5> :AsyncRun make dotest<cr>
" nnoremap <F6> :AsyncRun make parser && make && ./myparser mygood.cl <cr>
" nnoremap <F7> :AsyncRun make parser && make && ./myparser mygood.cl -p<cr>
" nnoremap <F8> :AsyncRun make parser && make && ./myparser mybad.cl <cr>
