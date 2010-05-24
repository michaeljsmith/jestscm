set ts=2 sw=2 lisp ai
let is_mzscheme=1

map <F5> :botr vnew<CR>:setlocal buftype=nofile bufhidden=delete noswapfile nowrap<CR>:r! mzscheme -l errortrace -u src/jest.ss<CR>

