autocmd!
set timeout timeoutlen=5000 ttimeoutlen=100
syntax on
set background=dark
set showmatch
set showtabline=1
set hidden
set nu
set nuw=4
set nowrap
set modifiable
set encoding=utf-8
set ff=unix
set nobomb
set wildmenu
set cmdheight=1
"set foldenable
"set foldmethod=indent
"set foldnestmax=1
nnoremap <space> za
vnoremap <space> zf


au WinLeave * set nocursorline "nocursorcolumn
au WinEnter * set cursorline "cursorcolumn
set cursorline "cursorcolumn
set colorcolumn=+1


" buffet
map <F2> :Bufferlist<CR>

" Disable this shit
imap <C-x><C-s> <Nop>
nmap <C-x><C-s> <Nop>

imap <up> <Nop>
imap <down> <Nop>
imap <left> <Nop>
imap <right> <Nop>
imap <insert> <Nop>
imap <delete> <Nop>
imap <home> <Nop>
imap <end> <Nop>
imap <PageDown> <Nop>
imap <PageUp> <Nop>
nmap <up> <Nop>
nmap <down> <Nop>
nmap <left> <Nop>
nmap <right> <Nop>
nmap <insert> <Nop>
nmap <delete> <Nop>
nmap <home> <Nop>
nmap <end> <Nop>
nmap <PageDown> <Nop>
nmap <PageUp> <Nop>
" tabulation options (4 spaces)
set expandtab
set shiftwidth=4
set softtabstop=4
set smarttab
set autoindent
set smartindent
set textwidth=72
set filetype=text
inoremap # X#
set list lcs=tab:>-,trail:-,extends:>
" current line
syntax match CurrentLine /.*\%#.*/
hi link CurrentLine Visual
au! BufNewFile,BufRead *.c,*.h,*.py,*.cc,*.cpp,*.rst exec 'match Todo /\%>' .  &textwidth . 'v.\+/'
" remove trailing whitespace
au! BufWritePre * :%s/\s\+$//e

" python
" im :<CR> :<CR><TAB>
au! BufRead,BufNewFile *.py set ts=4 sts=4 sw=4
au! BufRead,BufNewFile *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with
" ruby
au! BufRead,BufNewFile *.rb set smartindent ts=2 sts=2 sw=2
" go
au BufRead,BufNewFile *.go set smartindent
" sass
au! BufRead,BufNewFile *.sass setfiletype sass
" scss
au! BufRead,BufNewFile *.scss setfiletype scss
" scala
au! BufRead,BufNewFile *.scala set smartindent ts=2 sts=2 sw=2
" html
au! BufRead,BufNewFile *.html set smartindent ts=2 sts=2 sw=2
" vala
au! BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au! BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au! BufRead,BufNewFile *.vala setfiletype vala
au! BufRead,BufNewFile *.vapi setfiletype vala
" c
let c_cpp_comments = 1
let c_comment_strings = 1
let c_comment_numbers = 1
let c_comment_types = 1
let c_comment_date_time = 1
let c_no_comment_fold = 1
let c_warn_nested_comments = 1
let c_warn_8bitchars = 1
let c_warn_multichars = 1
let c_warn_digraph = 1
let c_warn_trigraph = 1
let c_char_is_integer = 1
let c_syntax_for_h = 1
let c_no_octal = 1
let c_ansi_typedefs = 1
let c_ansi_constants = 1
let c_impl_defined = 1
let c_C94 =1
let c_C99 =1
let c_posix = 1
let c_math = 1
let c_gnu = 1
let c_cpp_warn = 1
let c_conditional_is_operator = 1
"au! BufRead *.c set noexpandtab shiftwidth=8
"au! BufRead *.h set noexpandtab shiftwidth=8


filetype plugin on
"
" csv shit
hi! CSVColumnEven term=bold ctermbg=0 guibg=DarkBlue
hi! CSVColumnOdd  term=bold ctermbg=0 guibg=DarkMagenta


" Disable valadoc syntax highlight
" "let vala_ignore_valadoc = 0
"
" " Enable comment strings
" let vala_comment_strings = 1
"
" " Highlight space errors
" let vala_space_errors = 1
" " Disable trailing space errors
" "let vala_no_trail_space_error = 1
" " Disable space-tab-space errors
" let vala_no_tab_space_error = 1
"
" " Minimum lines used for comment syncing (default 50)
" "let vala_minlines = 120


"" omnicpp stuff
set nocp
filetype plugin on
command! Oc :!ctags -R -I --c++-kinds=+p --fields=+iaS --extra=+q .
command! Oh :!hasktags -c -x .
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif
set completeopt=menuone

"if &t_Co > 255
"    " More than 256 colors are available
"    colo solarized
"else
    colo solarized
"endif

cmap w!! w !sudo tee %

" Haskell Scion
" recommended: vim spawns a scion instance itself:
"let g:scion_connection_setting = [ 'scion', '/home/arch/x/tmp/scion.git/cabal-dev/bin/scion-server']

""-----------------------8<--------------------------
"function! SetToCabalBuild()
"  if glob("*.cabal") != ''
"    set makeprg=cabal\ build
"  endif
"endfunction
"
"autocmd BufEnter *.hs,*.lhs :call SetToCabalBuild()
""-----------------------8<--------------------------

autocmd BufEnter *.hs,*.lhs :set tw=80
