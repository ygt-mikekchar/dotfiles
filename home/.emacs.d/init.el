; Load ledger mode
(add-to-list 'load-path
             (expand-file-name "/home/mike/devel/ledger/lisp"))
(load "ledger-mode")
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
