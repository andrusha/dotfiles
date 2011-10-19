set t_Co=256
set nocompatible
set nobackup
set backupdir=~/.vim/backup
set directory=~/.vim/temp
set noautowrite
set autoread
set lazyredraw
set ttyfast
set modeline
set backspace=indent,eol,start

" Keymap & spellchecking for russian
" set keymap=russian-jcukenwin
" Langmap is better, because you can switch layout with OS bindings
set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯЖ;ABCDEFGHIJKLMNOPQRSTUVWXYZ:,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz
setlocal spell spelllang=ru_yo,en_us

" Remove intro message
set shortmess+=I

" Persistent undo vim >= 7.3
if has("persistend_undo")
    set undofile
    set undodir=~/.vim/temp/undo
endif

" Vundle setup
filetype off

set rtp+=~/.vim/vundle.git
call vundle#rc()

Bundle "wombat256.vim"
Bundle "Solarized"
Bundle "Markdown"

Bundle "surround.vim"
Bundle "Align"
Bundle "matchit.zip"
Bundle "delimitMate.vim"

Bundle "git.zip"
Bundle "Gist.vim"

Bundle "bufexplorer.zip"
Bundle "taglist.vim"
Bundle "The-NERD-tree"

Bundle "pyflakes"
Bundle "VimDebug"

Bundle "coffee.vim"
Bundle "vim-coffee-script"

Bundle "ZenCoding.vim"

Bundle "Haskell-Cuteness"
Bundle "syntaxhaskell.vim"
Bundle "indenthaskell.vim"

Bundle "SingleCompile"

" Turn on cool features
syntax on
filetype plugin indent on

" Interface
if has('gui_running')
    set background=dark
    let g:solarized_contrast="high"
    colors solarized
else
    colors wombat256mod
    hi ColorColumn guibg=#2d2d2d ctermbg=246
endif

set showcmd	    "partial commands
set showmatch	"brackets
set nocul	    "show line
set rnu	    	"relative line numbers
set ru		    "cursor position
set wildmenu	"command completion
set wildmode=list,full
set laststatus=2
set statusline=%F%m%r%h%w\ (%{&ff}){%Y}\ [%l,%v][%p%%]
set more	        "ask about continue list
set scrolloff=5     "5 lines to the up/down
set sidescrolloff=5
set linebreak	    "smart longlines breaking
set nostartofline   "better nav
set formatoptions=tcrql
set title
set textwidth=120
set colorcolumn=+1
set cursorline

" Tab line
set tabline=%!MyTabLine()
set showtabline=1

" gVim
set guioptions=a
set guifont=Inconsolata\ Medium\ 10

" Errors
set noerrorbells
set novisualbell
set t_vb=

" Search
set ignorecase
set smartcase
set incsearch
set nohlsearch

" Buffers
set nohidden	"close buffer

" Mouse
set mouse=a
set mousef
set mousem=popup

" Indentation
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smartindent
set autoindent
set smarttab
set listchars=tab:··,trail:»

" Folding
set foldmethod=indent
set foldminlines=2
set foldlevelstart=99
set foldnestmax=4

let g:clipbrdDefaultReg='+'

" New Tab
nnoremap <silent> <C-t> <ESC>:tabnew<CR>
nnoremap <silent> ,t <ESC>:tabnew<CR>

" NERDTree
nnoremap <silent> <F2> <ESC>:NERDTreeToggle<CR>

" BufExplorer
nnoremap <silent> <F3> <ESC>:BufExplorer<CR>

" Open the TagList Plugin
nnoremap <silent> <F4> <ESC>:Tlist<CR>

" Make
ino <silent> <F6> <ESC>:make -s<CR>
nno <silent> <F6> <ESC>:make -s<CR>

" Single Compile
nmap <silent> <F9> :SCCompile<cr>
nmap <silent> <F10> :SCCompileRun<cr>

" Session manager
nnoremap <silent> <F9> <ESC>:SessionOpen last<CR>
nnoremap <silent> <F10> <ESC>:SessionSaveAs last<CR>

" Folding on space
nnoremap <space> za

" Quickly set comma or semicolon at the end of the string
inoremap ,, <End>,
inoremap ;; <End>;
au FileType python inoremap :: <End>:
inoremap :: ::

" Make easier work in normal mode
nnoremap ; :
nnoremap , ;

" Easy copy-paste from clipboard
nnoremap <silent> ,p "+p
nnoremap <silent> ,P "+P
nnoremap <silent> ,y "+yy

" Shift on Tab
nnoremap <Tab> >>
nnoremap <S-Tab> <<
vnoremap <Tab> >
vnoremap <S-Tab> <

" Smart home (go to real start of line)
nnoremap <silent> <Home> g^

" Tab navigation with Ctrl + Tabnum
ino <silent> <C-1> <ESC>:tabfirst<CR>
nno <silent> <C-1> <ESC>:tabfirst<CR>
ino <silent> <C-2> <ESC>2gt
nno <silent> <C-2> <ESC>2gt
ino <silent> <C-3> <ESC>3gt
nno <silent> <C-3> <ESC>3gt
ino <silent> <C-4> <ESC>4gt
nno <silent> <C-4> <ESC>4gt
ino <silent> <C-5> <ESC>5gt
nno <silent> <C-5> <ESC>5gt
ino <silent> <C-6> <ESC>6gt
nno <silent> <C-6> <ESC>6gt
ino <silent> <C-7> <ESC>7gt
nno <silent> <C-7> <ESC>7gt
ino <silent> <C-8> <ESC>8gt
nno <silent> <C-8> <ESC>8gt
ino <silent> <C-9> <ESC>:tablast<CR>
nno <silent> <C-9> <ESC>:tablast<CR>

" arrow keys move visible lines
inoremap <silent> <Down> <C-R>=pumvisible() ? "\<lt>Down>" : "\<lt>C-O>gj"<CR>
inoremap <silent> <Up> <C-R>=pumvisible() ? "\<lt>Up>" : "\<lt>C-O>gk"<CR>
nnoremap <silent> <Down> gj
nnoremap <silent> <Up> gk
vnoremap <silent> <Down> gj
vnoremap <silent> <Up> gk

" buffer navigation with Ctrl+j, Ctrl+k
nnoremap <silent> <C-j> <C-w>j
nnoremap <silent> <C-k> <C-w>k

" Ctrl+Backspace, Ctrl+Delete word deletion
inoremap <silent> <C-Backspace> <ESC><right>dbi
inoremap <silent> <C-Delete>    <ESC><right>dwi

" Save file by Ctrl+S
inoremap <silent> <C-S> <ESC>:w<CR>i
nnoremap <silent> <C-S> <ESC>:w<CR>i
vnoremap <silent> <C-S> <ESC>:w<CR>i

vnoremap <silent> <BS> d

" Open .vimrc
map ,v :vsp $MYVIMRC<CR>
map ,V :source $MYVIMRC<CR>

" Translate markdown to html
nmap <silent> ,md :%!/usr/local/bin/markdown.pl --html4tags <cr>

function! MyTabLine()
    let s = '%#TabLine#'
    for i in range(tabpagenr('$'))
        let k = i + 1
        let curbuf = tabpagebuflist(k)[tabpagewinnr(k) - 1]
        let select = k == tabpagenr()

        let fname = bufname(curbuf)
        if fname == ''
            let fname = '[No File]'
        else
            let fname = fnamemodify(fname, ':p:.')
            let fname = substitute(fname, '\(\w\)\w*\/', '\1\/', 'g')
        endif

        let s .= select ? '%#TabLineSel#' : ''
        let s .= '%'.k .'T '.k.' '.fname
        let s .= getbufvar(curbuf, '&mod') ? ' +' : ''
        let s .= ' '.(select ? '%#TabLine#' : ''). '|'
    endfor

    let s = strpart(s, 0, strlen(s)-1)
    let s .= '%#TabLineFill#%T'
    if tabpagenr('$') > 1
        let s .= '%=%#TabLine#%999X X'
    endif

    return s
endfunction

" TagList plugin settings
let Tlist_Use_Right_Window = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Exit_OnlyWindow = 1
let Tlist_Use_SingleClick = 1
let Tlist_Inc_Winwidth = 0
let Tlist_Show_One_File = 1

" NERDTree settings
let NERDTreeIgnore = ['\.pyc$', '\.pyo$']

" Write with sudo
command! W w !sudo tee % > /dev/null

" Automatically add header on new files
autocmd BufNewFile *.sh s-^-#!/bin/bash\r\r-
autocmd BufNewFile *.py s-^-#!/usr/bin/python\r\r-
autocmd BufNewFile *.rb s-^-#!/usr/bin/ruby\r\r-

" Fix memory leak
au BufWinLeave * call clearmatches()

au BufRead,BufEnter {/home/andrew/projects/TopTal/*,~/projects/TopTal/*} set noet list
