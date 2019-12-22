set tags+=../../include/PA4/.tags
set path+=../../include/PA4/**

set makeprg=make\ dotest
nnoremap <F3> :AsyncRun make<cr>
nnoremap <F4> :AsyncRun make semant && ./mysemant -s mygood.cl<cr>
nnoremap <F5> :AsyncRun make semant && ./mysemant -s mybad.cl<cr>
nnoremap <F6> :AsyncRun make dotest <cr>
" nnoremap <F7> :AsyncRun make parser && make && ./myparser mygood.cl -p<cr>
" nnoremap <F8> :AsyncRun make parser && make && ./myparser mybad.cl <cr>

autocmd BufRead,BufNewFile *.test set syntax=cool filetype=cool
