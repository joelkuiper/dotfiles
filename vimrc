" vimrc


" Author: Zaiste! <oh@zaiste.net>
" Source: https://github.com/zaiste/vimified
"
" Have fun!
"

set nocompatible
filetype off

" Load external configuration before anything else {{{
if filereadable(expand("~/.vim/before.vimrc"))
  source ~/.vim/before.vimrc
endif
" }}}

let mapleader = ","
let maplocalleader = "\\"

" Local vimrc configuration {{{
let s:localrc = expand($HOME . '/.vim/local.vimrc')
if filereadable(s:localrc)
  exec ':so ' . s:localrc
endif
" }}}

" PACKAGE LIST {{{
" Use this variable inside your local configuration to declare
" which package you would like to include
if ! exists('g:vimified_packages')
  let g:vimified_packages = ['general', 'os', 'coding', 'color']
endif
" }}}

" VUNDLE {{{
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
" }}}

" PACKAGES {{{

" _. General {{{
if count(g:vimified_packages, 'general')
  if has("gui")
    set guioptions-=T
    set guioptions-=r
    set guioptions-=m
  endif
endif
" }}}

" _. OS {{{
if count(g:vimified_packages, 'os')
  set clipboard=unnamed

  if $COLORTERM == 'gnome-terminal'
    set t_Co=256
  endif
endif
" }}}

" _. Coding {{{
if count(g:vimified_packages, 'coding')
  au InsertEnter * :set nu
  au InsertLeave * :set rnu
  au FocusLost * :set nu
  au FocusGained * :set rnu

  autocmd FileType gitcommit set tw=68 spell colorcolumn=68
  autocmd FileType gitcommit setlocal foldmethod=manual
endif
" }}}

" _. Color {{{
if count(g:vimified_packages, 'color')
  syntax on
  set background=light
endif
" }}}

" General {{{
filetype plugin indent on

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" }}}

" Mappings {{{

" w!! to write a file as sudo
" stolen from Steve Losh
cmap w!! w !sudo tee % >/dev/null

" Seriously, guys. It's not like :W is bound to anything anyway.
command! W :w

" }}}

" Settings {{{
set nocursorcolumn
set nocursorline

set autoread
set backspace=indent,eol,start
set binary
set scrolloff=5

set ttyfast
set lazyredraw

set hidden
set incsearch
set laststatus=2
set encoding=UTF-8

set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set showbreak=↪

set noeol
set numberwidth=10
set number
set showcmd

" White characters {{{
  set autoindent
  set copyindent
  set tabstop=2
  set softtabstop=2
  set textwidth=80
  set shiftwidth=2
  set shiftround
  set expandtab
  set formatoptions=qrn1
  set colorcolumn=+1
" }}}

" Trailing whitespace {{{
" Only shown when not in insert mode so I don't go insane.
augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:⌴
    au InsertLeave * :set listchars+=trail:⌴
augroup END

" }}}

set wildignore=.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,.DS_Store,*.aux,*.out,*.toc
set wildmenu

set dictionary=/usr/share/dict/words

" Triggers {{{

" Remove trailing whitespaces when saving
" Wanna know more? http://vim.wikia.com/wiki/Remove_unwanted_spaces
" If you want to remove trailing spaces when you want, so not automatically,
" see
" http://vim.wikia.com/wiki/Remove_unwanted_spaces#Display_or_remove_unwanted_whitespace_with_a_script.
autocmd BufWritePre * :%s/\s\+$//e

" Save when losing focus
au FocusLost    * :silent! wall

" }}}

" Searching {{{

set ignorecase
set smartcase

" }}}

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Quick editing {{{

nnoremap <leader>ev <C-w>s<C-w>j:e $MYVIMRC<cr>
nnoremap <leader>ez <C-w>s<C-w>j:e ~/.zshrc<cr>

" }}}

" Vim {{{
augroup ft_vim
  au!
  au FileType vim setlocal foldmethod=marker
  au FileType help setlocal textwidth=78
  au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END
" }}}

" }}}

" EXTENSIONS {{{


" }}}

" Load addidional configuration (ie to overwrite shorcuts) {{{
if filereadable(expand("~/.vim/after.vimrc"))
  source ~/.vim/after.vimrc
endif
" }}}