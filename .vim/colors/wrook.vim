set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "wrook"
"k r g y b v c w"
if has("gui_running")
  let s:vmode       = "gui"
  let s:i           = "italic"
  let s:ii          = ",italic"

  let s:border      = "#c0c0c0"
  let s:diffdelete  = "#f04070"
  let s:diffadd     = "#50f040"
  let s:nontext     = "#805010"
  let s:stringback  = "#b5d5c5"
  let s:unused      = "#daaef1"
  let s:difftext    = "#08e0c0"
  let s:storage     = "#0248a2"

  let s:text        = "#202040"
  let s:comment     = "#a02040"
  let s:constant    = "#206060"
  let s:highlight   = "#f0f020"
  let s:statement   = "#104070"
  let s:preproc     = "#705070"
  let s:label       = "#209090"
  let s:background  = "#f7f7d5"
else
  let s:vmode       = "cterm"
  let s:i           = "none"
  let s:ii          = ""

  let s:border      = "0" 
  let s:diffdelete  = "1"
  let s:diffadd     = "2"
  let s:nontext     = "3"
  let s:stringback  = "4"
  let s:unused      = "5"
  let s:difftext    = "6"
  let s:storage     = "7"

  let s:text        = "8"
  let s:comment     = "9"
  let s:constant    = "10"
  let s:highlight   = "11"
  let s:statement   = "12"
  let s:preproc     = "13"
  let s:label       = "14"
  let s:background  = "15"
endif

exe "hi! Normal       ".s:vmode."fg=".s:text."      ".s:vmode."bg=".s:background
exe "hi! Function     ".s:vmode."fg=".s:label." ".s:vmode."=bold"
exe "hi! NonText      ".s:vmode."fg=".s:nontext
exe "hi! comment      ".s:vmode."fg=".s:comment." ".s:vmode."=bold".s:ii
exe "hi! constant     ".s:vmode."fg=".s:constant." ".s:vmode."=bold"
exe "hi! identifier   ".s:vmode."fg=".s:text." ".s:vmode."=bold"
exe "hi! statement    ".s:vmode."fg=".s:statement." ".s:vmode."=bold"
exe "hi! preproc      ".s:vmode."fg=".s:preproc." ".s:vmode."=bold"
exe "hi! type         ".s:vmode."fg=".s:text." ".s:vmode."=bold"
exe "hi! special      ".s:vmode."fg=".s:label." ".s:vmode."=bold"
exe "hi! string       ".s:vmode."fg=".s:statement." ".s:vmode."bg=".s:stringback
exe "hi! ErrorMsg     ".s:vmode."fg=".s:comment."   ".s:vmode."bg=".s:highlight
exe "hi! WarningMsg   ".s:vmode."fg=".s:label."     ".s:vmode."bg=".s:highlight
exe "hi! Error        ".s:vmode."fg=".s:comment."   ".s:vmode."bg=".s:highlight
exe "hi! Todo         ".s:vmode."fg=".s:highlight." ".s:vmode."bg=".s:comment." ".s:vmode."=bold"
exe "hi! Cursor       ".s:vmode."fg=".s:highlight." ".s:vmode."bg=".s:comment
exe "hi! Search       ".s:vmode."fg=".s:label."     ".s:vmode."bg=".s:highlight
exe "hi! InSearch                           ".s:vmode."bg=".s:highlight." ".s:vmode."=NONE"
exe "hi! LineNr       ".s:vmode."fg=".s:statement." ".s:vmode."bg=".s:border." ".s:vmode."=bold"
exe "hi! StatusLineNC ".s:vmode."fg=".s:statement." ".s:vmode."bg=".s:border." ".s:vmode."=NONE"
exe "hi! StatusLine   ".s:vmode."fg=".s:highlight." ".s:vmode."bg=".s:statement." ".s:vmode."=bold"
exe "hi! VertSplit    ".s:vmode."fg=".s:statement." ".s:vmode."=reverse"
exe "hi! label        ".s:vmode."fg=".s:label." ".s:vmode."=bold"
exe "hi! operator     ".s:vmode."fg=".s:text." ".s:vmode."=bold"

hi link cPreCondit preproc
exe "hi! cDefine      ".s:vmode."fg=".s:label." ".s:vmode."=bold"
exe "hi! cStructure   ".s:vmode."fg=".s:statement." ".s:vmode."=bold"
hi link cppStructure cStructure
hi link cCommentL comment
hi link Type text
hi link cppType text
exe "hi! cStorageClass   ".s:vmode."fg=".s:statement." ".s:vmode."=bold".s:ii
hi link cInclude cDefine
exe "hi! cIncluded    ".s:vmode."fg=".s:label." ".s:vmode."=bold"
hi link cNumber constant
hi link cppBoolean constant
hi link cUserLabel label
hi link cString string
hi link cComment comment
hi link cStatement text
hi link cConditional text
hi link cConstant constant
hi link cOperator constant

hi clear Visual
exe "hi! Visual ".s:vmode."fg=".s:highlight."   ".s:vmode."bg=".s:text." ".s:vmode."=reverse"
exe "hi! DiffChange                     ".s:vmode."bg=".s:stringback
exe "hi! DiffText                       ".s:vmode."bg=".s:difftext
exe "hi! DiffAdd                        ".s:vmode."bg=".s:diffadd
exe "hi! DiffDelete                     ".s:vmode."bg=".s:diffdelete
hi link Folded StatusLine
exe "hi! FoldColumn   ".s:vmode."fg=".s:border." ".s:vmode."bg=".s:diffdelete

exe "hi! rubyFunction ".s:vmode."fg=".s:highlight." ".s:vmode."bg=".s:label." ".s:vmode."=bold"
exe "hi! rubyInstanceVariable  ".s:vmode."=".s:i
exe "hi! rubySymbol ".s:vmode."fg=".s:statement." ".s:vmode."=bold"

exe "hi! javaScriptFuncName ".s:vmode."fg=".s:highlight." ".s:vmode."bg=".s:label." ".s:vmode."=bold"

