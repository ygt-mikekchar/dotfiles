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
let g:syntastic_javascript_checkers = ['jsxhint']
let g:syntastic_ruby_checkers = ['rubocop']

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

if has('nvim')
  call plug#begin()
  Plug 'neovim/nvim-lsp'
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'Shougo/deoplete-lsp'
  Plug 'ervandew/supertab'
  Plug 'Chiel92/vim-autoformat'
  call plug#end()

  " setup LSP (IDE features)
  lua require'nvim_lsp'.rust_analyzer.setup{}
  lua require'nvim_lsp'.gopls.setup{}
  lua require'nvim_lsp'.solargraph.setup{}
  "
  " Use LSP omni-completion in Rust files
  autocmd Filetype rust setlocal omnifunc=v:lua.vim.lsp.omnifunc

  " Enable deoplete autocompletion
  let g:deoplete#enable_at_startup = 1

  " customise deoplete
  " maximum candidate window length
  call deoplete#custom#source('_', 'max_menu_width', 80)

  " Press Tab to scroll _down_ a list of auto-completions
  let g:SuperTabDefaultCompletionType = "<c-n>"

  " rustfmt on write using autoformat
  let g:formatdef_rustfmt = '"rustfmt +nightly --edition 2018"'
  autocmd BufWrite * if &ft==?'rust'|:Autoformat|endif

  nnoremap <leader>c :!cargo clippy
  nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
  nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
  nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
  nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
  nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
  nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
  nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
  nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
  nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
endif
