;;; init -- Mike's init.el file

;; Author: Mike Charlton

;;; Commentary:

;; Basically trying to keep this as simple as possible.  The intent is to provide
;; the simplest thing that will get me able to use Evil mode in roughly
;; the same way that I use Vim.

;;; Code:
;; packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defvar mkc/emacs-library-dir (expand-file-name (concat user-emacs-directory "lisp"))
    "The directory where various unpackaged libraries are living.")

;; Add library directory recursively
(let ((default-directory mkc/emacs-library-dir))
    (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))

;; Set up default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Useful function for deleting all buffers
(defun close-all-buffers ()
  "Delete all active buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; turn off menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; disable vc-git -- it is giving me errors
(setq vc-handled-backends ())

;; turn off blinking cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; turn on line numbers by default
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;; Format the line numbers to be 3 digits followed by a space
(setq linum-format "%3d ")
;; show column nubers in the mode-line
(setq column-number-mode t)

;; Use the directory where Emacs was first started as the
;; directory to open new files (the same as Vim)
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))

(use-package company
  :ensure t
  :config
  (global-company-mode))

;; This doesn't really play nicely with the latest emacs :-(
;; gwt mode for java files
;; (autoload 'gwt-mumamo-mode "gwt-mumamo" "" t)
;; (add-to-list 'auto-mode-alist '("\.java$" . gwt-mumamo-mode))
;; mumamo changes the background color.  This will make
;; mumamo color only inline code
;;(setq mumamo-chunk-coloring 1)

(use-package jtags
  :ensure t
  :config
  (add-hook 'java-mode-hook 'jtags-mode)
  (lambda ()
    (define-key jtags-mode-map "\M-n" `jtags-member-completion)))

(setq tags-revert-without-query 't)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Load IDO mode for buffer completion
(require 'ido)
(ido-mode t)

;; Load evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-ex-define-cmd "bw[ipeout]" 'kill-buffer)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key evil-ex-map "e " 'ido-find-file)
  (define-key evil-ex-map "b " 'ido-switch-buffer)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1)
  )

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g i" . dumb-jump-go-prompt)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :ensure t
  :config
  (dumb-jump-mode)
  (setq dumb-jump-default-project "~/work/sales"))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(use-package json-mode
  :ensure t
  )

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (eval-after-load 'go-mode
    '(add-hook 'before-save-hook 'gofmt-before-save)))

(use-package coffee-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

(use-package flycheck
  :ensure t
  :pin melpa
  :config
  (setq flycheck-ruby-rubocop-executable "/usr/local/bin/rubocop")
  (global-flycheck-mode))

(use-package magit
  :ensure t)

;; (with-eval-after-load 'magit
;;  (load "~/devel/evil-magit/evil-magit.el"))
(use-package evil-magit
  :ensure t
  :config
  (global-set-key "\C-xg" 'magit-status))

;; Set up some global keybindings for Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Add time stamp to "DONE" items
(setq org-log-done t)

;; Paths for agenda and notes
(setq org-agenda-files (list "~/org" "~/work/journal/mike-journal.org"))
(setq org-default-notes-file "~/org/notes.org")

;; Always use relative paths for org mode links
(setq org-link-file-path-type 'relative)

;; Use line numbers for storing links
(defun link-to-line-number ()
  "Link to line numbers when making org mode links."
  (number-to-string (org-current-line)))
(add-hook 'org-create-file-search-functions
	  'link-to-line-number)


;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (agnostic)))
 '(custom-safe-themes
   (quote
    ("f8f57bb990c60e042ad04a16ec7e2e06b2329753f0df4db96265697492272b1f" "73012a2f6cbf3645fa019ca2801d7561ab36474b35f1a92291e9ff8ad26164d8" "ceeb8d1c88030c65a60d4b4306f2ab104a5ef0d448f434dfd0e437870fe394c4" "8b927b551e0332035fc8694236f398c149dec629b3927167c12fcaddd8058de4" "f0352ed0b1c5ca5159e5de89c1456cbac9d582ce7c8600c786b90200048c7496" "5acf9a312617a4d269f08438154b51608f3e5bb5bb6d1ff6a68345532735b578" "c8dc6cce29d47f4f35e2fa6c92db37dae7ec40c43f44bc3d5449f4e340b3f292" "568c0f257e444904c3d9ec7174c958bed4dec19684964f6d9b689544a1a9a6ae" "24eabb04960aa2b52371307b373ee9efc91c8c98d137448e7c023c18d79ce86c" "f1ce2f12ddfc4ea2be9d6ea997971d59f136b78930e0c3bd07a8bdb7d639c6d9" "7779e67feefc8a42ce9cc65e2a4e1afad79718a9d6fd9a28095a9132a62e995f" "1fa142653c10fa72a8fda151f733b7b2ae4bb26a72899e60ea9cd01a24d4023a" "3ae63b5300acc68006a0a9cf251907811182566e2ff992453a7016d7e4d431b7" "1722b8321e082e903006732d1b4a890dc8bf994e5f49df9c96442e798db77c1b" "cd314e07214617a4cf31b7f89c26f2f582bea0168e32ccb59a9b611d39d67956" "6d012ea4d872f0f3e3cd9ef2784ca3a699097fc704337e5ba090c918c5ae31c9" "684e38c64081cd699bb6a723cdeb2fd49bb1ae767aaf0dcb63ba10e28630a985" "c9f3de9842cb5a1244fb79354c415b4f64aac85ccec51111be0260773362fe66" "ae9a2953025f3c2837d35ad2f7910c2424063ae48f13293774d43c90a6dca15e" "6603c34e81fd25b59d6329e4e8d0b083fcd24c50ddcdd94bb4fcd2ccc8da0263" "e39769e198080195c07f3a08dad352298c4c1085b0c5266883bbc8b9db9cb11c" "2af32cc56ac6e544c9661ce4ab60ffe55940a1564031b91af8989c66cf62f177" "81f31984aa99ab4b1be4a73f7b5a2e2d134a8e3107266bd8fb93683d59d8c0ce" "8189204b074586475ac8d1be028bbdc4e0d1256cf2c063b706b58c38b5173016" "a1ed894e04510d07b4b6cf03267258357afdac6c7d824ab5d45428eda2d57a0a" "bf44f7d22051ac9478aa153ba50199ed0fbe359e1c1854fe7440dc000a6e1228" "cccd52d7943e895c3c850d9f49298872cb57c51f5b73349ef3b2bc3283b9f3f5" "362d2dc4e0fa755e0c1009ed0c37a206c92dc7bda0ded39cec8df32d13a98b0a" "c8b27c432c3983f4e703c592327bf0d10ff890e3054d47ee471c5f7d4dc5aef8" "65f8d6345f5898b1ee86b885fd61e2ce85f3589722e0cabe8df7cb4daa3e88b5" "81d75a80856e7368952dd3408c6cdb6935f1d6199579d372ae9004b7307bbe4b" "736e0868e57d6e29f3b1cc29c8edc7b62692ae4e1dd1429f9ecd934ba0f8a697" "5d97cce1063d40c95eda54bb69eb42ed05e839313d181c49464679e38285232d" "5d92dcbb0ceb8fe059732fce6d433c77ec3f8b439e9e2f6bbfb947df89b89979" "f51a4d4328c4a235942d819cd71f6b0c761296bc2cfc2cde44f33bbfc0711e05" "df61a8a15e9aac1f9925c452adfa532ec14f07ab40d7097617a0e7cbb4456fb6" "f89abc033d8b27200f38ecb7e3dd9b9ae6100211cdd7e489c1e86318dbaa0a9e" "537cfac1c3b28b04bc41341abb5857f9549905718abb540fe67accd7b3bdb73f" "ad6d43807539295bb05aab6577b41776deaff984aede94432961b2de59265abf" default)))
 '(package-selected-packages
   (quote
    (web-mode dumb-jump json-mode flymake-go company-go go-mode flycheck-haskell haskell-mode company flycheck-elm eslint-fix js2-mode jtags purescript-mode flycheck-purescript elm-mode enh-ruby-mode magit ledger-mode flycheck evil editorconfig coffee-mode)))
 '(ruby-deep-indent-paren nil))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
