autocmd!
syntax on
set background=dark
set showmatch
set showtabline=2
set nu
set nuw=4
set cursorline
set nowrap
set modifiable
set encoding=utf-8
set ff=unix
set nobomb
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
set textwidth=120
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
""set nocp
""filetype plugin on
""command! Oc :!ctags -R -I --c++-kinds=+p --fields=+iaS --extra=+q .
""autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
""autocmd InsertLeave * if pumvisible() == 0|pclose|endif
""set completeopt=menuone

if &t_Co > 255
    " More than 256 colors are available
    colo zenburn
else
    colo torte
endif

cmap w!! w !sudo tee %
