execute pathogen#infect()
syntax enable
filetype plugin indent on
set number
set fillchars+=vert:\
set hlsearch

" set tab stops to 2 and expand tabs to spaces
set ts=2
set sw=2
set expandtab

" turn on ruby evaluation for c-x c-o expansion
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
let g:rubycomplete_rails = 1

" Options for Go
filetype off
set runtimepath+=$GOROOT/misc/vim
filetype plugin indent on
syntax on
augroup customGo
  autocmd!
  autocmd BufWritePre *.go Fmt
  autocmd BufWritePost *.go call VimuxRunCommand("go test ./...")
augroup END
autocmd FileType go set noexpandtab

" Options for Vimux
let mapleader = ","
let VimuxUseNearestPane = 1
map <Leader>rp :VimuxPromptCommand<CR>
map <Leader>rr :VimuxRunLastCommand<CR>
map <leader>n :NERDTreeToggle<cr>

" GUI
if has("gui_running")
  set gfn=Anonymous\ Pro\ 12
  set guioptions-=T

  " Turn off annoying popups in ruby mode
  set noballooneval

  " Window size and position
  " Set some things differently for vimdiff
  if &diff
    set lines=52
    " Enough space for 2 panes side by side
    set columns=167
    winpos 217 71
  else
    set lines=52
    " Enough space for NERDTree
    set columns=136
    winpos 62 85
  endif

  " Default colors
  colorscheme wrook
else
  " Disable Background Color Erase (BCE) so that color schemes
  " work properly when Vim is used inside tmux and GNU screen.
  " See also http://snk.tuxfamily.org/log/vim-256color-bce.html
  if &term =~ '256color'
    set t_ut=
  endif

  " Default colors
  set t_Co=16
  let g:solarized_termcolors=16
  colorscheme agnostic
"  set background=dark
"  colorscheme solarized
"  colorscheme wrook

  " Setting up the mouse for console work
  set ttyfast
  set mouse=a
  set ttymouse=xterm2
  set mousehide
  set mousemodel=popup
endif
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

