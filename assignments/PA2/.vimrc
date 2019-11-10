set tags+=../../include/PA2/.tags
set path+=../../include/PA2/**

set makeprg=make\ dotest
nnoremap <F4> :AsyncRun make dotest<cr>
