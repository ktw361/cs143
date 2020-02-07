set ts=2
set shiftwidth=2

autocmd BufRead,BufNewFile *.cl set cms=--\ %s
autocmd BufRead,BufNewFile *.s set cms=#\ %s

set tags+=../../include/PA5/.tags
set path+=../../include/PA5/**

set makeprg=make\ dotest
nnoremap <F3> :AsyncRun make<cr>
nnoremap <F4> :AsyncRun make cgen<cr>
" nnoremap <F4> :AsyncRun make semant && ./mysemant -s mygood.cl<cr>
" nnoremap <F5> :AsyncRun make semant && ./mysemant -s mybad.cl<cr>
if has('macunix')
  nnoremap <F6> :AsyncRun make cgen && ./mycoolc -c example.cl && ./macspim example.s <cr>
endif
" nnoremap <F7> :AsyncRun make parser && make && ./myparser mygood.cl -p<cr>
" nnoremap <F8> :AsyncRun make parser && make && ./myparser mybad.cl <cr>

autocmd BufRead,BufNewFile *.test set syntax=cool filetype=cool
