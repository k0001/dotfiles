" All system-wide defaults are set in $VIMRUNTIME/archlinux.vim (usually just
" /usr/share/vim/vimfiles/archlinux.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vimrc), since archlinux.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing archlinux.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages.
runtime! archlinux.vim

" If you prefer the old-style vim functionalty, add 'runtime! vimrc_example.vim'
" Or better yet, read /usr/share/vim/vim72/vimrc_example.vim or the vim manual
" and configure vim to your own liking!


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
au BufNewFile,BufRead *.c,*.h,*.py,*.cc,*.cpp,*.rst exec 'match Todo /\%>' .  &textwidth . 'v.\+/'
" python
" im :<CR> :<CR><TAB>
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class,with
" ruby
autocmd BufRead *.rb set ts=2 sts=2 sw=2
" scala
autocmd BufRead *.scala set smartindent ts=2 sts=2 sw=2
" remove trailing whitespace
autocmd BufWritePre * :%s/\s\+$//e
" sass
"au! BufRead,BufNewFile *.sass         setfiletype sass

" omnicpp stuff
set nocp
filetype plugin on
command! Oc :!ctags -R -I --c++-kinds=+p --fields=+iaS --extra=+q .
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif
set completeopt=menuone

if &t_Co > 255
    " More than 256 colors are available
    colo ir_black
    "colo las
else
    colo torte
endif

cmap w!! w !sudo tee %
