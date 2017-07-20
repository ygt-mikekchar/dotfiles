execute pathogen#infect()
syntax enable
filetype plugin indent on
set number
set fillchars+=vert:\  
set hlsearch

map <leader>s :execute "noautocmd vimgrep /" . expand("<cword>")  . "/j **/" . expand("%e") <BAR> cw<CR>

" set tab stops to 2 and expand tabs to spaces
set ts=2
set sw=2
set expandtab

" Use tabs for java files
au FileType java setlocal ts=4 sts=4 sw=4 noexpandtab
au FileType xml setlocal ts=4 sts=4 sw=4 noexpandtab

" turn on ruby evaluation for c-x c-o expansion
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
let g:rubycomplete_rails = 1

" Options for Go
filetype off
set runtimepath+=/usr/lib/go/misc/vim
filetype plugin indent on
syntax on
autocmd FileType go set noexpandtab

" Options for Haml
autocmd FileType haml set noexpandtab

" Options for Vimux
let VimuxUseNearestPane = 1
map <Leader>rp :VimuxPromptCommand<CR>
map <Leader>rr :VimuxRunLastCommand<CR>
map <leader>n :NERDTreeToggle<cr>

" Options for Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_ruby_rubocop_exec = '/usr/local/bin/rubocop'

" Start and stop thyme
nmap <leader>t :!thyme -d<cr>
nmap <leader>T :!thyme -s<cr>

" GUI
" Disable Background Color Erase (BCE) so that color schemes
" work properly when Vim is used inside tmux and GNU screen.
" See also http://snk.tuxfamily.org/log/vim-256color-bce.html
if &term =~ '256color'
  set t_ut=
endif

" Default colors
set t_Co=256
colorscheme agnostic

" Don't rename files when making backups (it screws up webpack)
:set backupcopy=yes

" Setting up the mouse for console work
set ttyfast
"set mouse=a
"set ttymouse=xterm2
"set mousehide
"set mousemodel=popup

" Org mode setup
let g:org_heading_shade_leading_stars = 0
let g:org_indent = 0

" Setup for literate coffee
autocmd FileType litcoffee runtime ftplugin/coffee.vim
autocmd FileType cjsx.md runtime ftplugin/coffee.vim
