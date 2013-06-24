set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "wrook"

let s:background  = "#f7f7d5"
let s:text        = "#001020"
let s:stringback  = "#b5d5c5"
let s:highlight   = "#f0f020"
let s:comment     = "#a02040"
let s:label       = "#209090"
let s:statement   = "#104070"
let s:type        = "#304060"
let s:border      = "#c0c0c0"

exe "hi! Normal       guifg=".s:text."         guibg=".s:background
exe "hi! Function     guifg=".s:label."        gui=bold"
hi NonText      guifg=#301010
exe "hi! comment      guifg=".s:comment."      gui=bold,italic"
hi constant     guifg=#206060                   gui=bold
exe "hi! identifier   guifg=".s:type."         gui=bold"
exe "hi! statement    guifg=".s:statement."  gui=bold"
hi preproc      guifg=#705070                   gui=bold
exe "hi! type         guifg=".s:type."         gui=bold"
exe "hi! special      guifg=".s:label."      gui=bold"
exe "hi! string       guifg=".s:statement."  guibg=".s:stringback
exe "hi! ErrorMsg     guifg=".s:comment."  guibg=".s:highlight
exe "hi! WarningMsg   guifg=".s:label."    guibg=".s:highlight
exe "hi! Error        guifg=".s:comment."  guibg=".s:highlight
exe "hi! Todo         guifg=".s:highlight."  guibg=".s:comment." gui=bold"
exe "hi! Cursor       guifg=".s:highlight."   guibg=".s:comment
exe "hi! Search       guifg=".s:label."   guibg=".s:highlight
exe "hi! InSearch                           guibg=".s:highlight." gui=NONE"
exe "hi! LineNr       guifg=".s:statement."  guibg=".s:border." gui=bold"
exe "hi! StatusLineNC guifg=".s:statement."  guibg=".s:border." gui=NONE"
exe "hi! StatusLine   guifg=".s:highlight."  guibg=".s:statement." gui=bold"
exe "hi! VertSplit    guifg=".s:statement."      gui=reverse term=reverse cterm=reverse"
exe "hi! label        guifg=".s:label."          gui=bold"
exe "hi! operator     guifg=".s:type."           gui=bold"

hi link cPreCondit preproc
hi cDefine      guifg=#209090                   gui=bold
hi cStructure   guifg=#104070                   gui=bold
hi link cppStructure cStructure
hi link cCommentL comment
hi link Type type
hi link cppType type
hi cStorageClass guifg=#404040                  gui=bold,italic
hi link cInclude cDefine
hi cIncluded    guifg=#209090                   gui=NONE
hi link cNumber constant
hi link cppBoolean constant
hi link cUserLabel label
hi link cString string
hi link cComment comment
hi link cStatement type
hi link cConditional type
hi link cConstant constant
hi link cOperator constant

hi clear Visual
hi Visual     guifg=#f0f020   guibg=#304060 gui=reverse term=reverse cterm=reverse
hi DiffChange                 guibg=#b0ffc0
hi DiffText                   guibg=#08e0c0
hi DiffAdd                    guibg=#50f040
hi DiffDelete                 guibg=#f04070
hi link Folded StatusLine
hi FoldColumn guifg=white    guibg=gray30

exe "hi! rubyFunction guifg=".s:highlight."  guibg=".s:label." gui=bold"
exe "hi! rubyInstanceVariable  gui=italic"

exe "hi! javaScriptFuncName guifg=".s:highlight."  guibg=".s:label." gui=bold"



