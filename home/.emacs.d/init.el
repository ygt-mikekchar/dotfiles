;;; init -- Mike's init.el file

;; Author: Mike Charlton

;;; Commentary:

;; Basically trying to keep this as simple as possible.  The intent is to provide
;; the simplest thing that will get me able to use Evil mode in roughly
;; the same way that I use Vim.

;;; Code:
(defvar mkc/emacs-library-dir (expand-file-name (concat user-emacs-directory "lisp"))
    "The directory where various unpackaged libraries are living")

;; Add library directory recursively
(let ((default-directory mkc/emacs-library-dir))
    (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)
(require 'package)

;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; turn off menu bar
(menu-bar-mode -1)

;; disable vc-git -- it is giving me errors
(setq vc-handled-backends ())

;; set indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'ruby-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'coffee-tab-width 'tab-width)

;; turn off blinking cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; turn on line numbers by default
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
;; Format the line numbers to be 3 digits followed by a space
(setq linum-format "%3d ")

;; Use the directory where Emacs was first started as the
;; directory to open new files (the same as Vim)
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; Install a package from the archives if it doesn't exist, otherwise load it.
;; Code from: http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
(defun require-package (package)
  "Install given PACKAGE from package archives."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'ledger-mode)

(setq evil-want-C-i-jump nil)
(require-package 'evil)
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "b[wipeout]" 'kill-buffer))

(require-package 'coffee-mode)

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Turn on Evil mode
(evil-mode 1)

;; Set up some global keybindings for Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-agenda-files (list "~/org" "~/work/journal/mike-journal.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (agnostic)))
 '(custom-safe-themes
   (quote
    ("2af32cc56ac6e544c9661ce4ab60ffe55940a1564031b91af8989c66cf62f177" "81f31984aa99ab4b1be4a73f7b5a2e2d134a8e3107266bd8fb93683d59d8c0ce" "8189204b074586475ac8d1be028bbdc4e0d1256cf2c063b706b58c38b5173016" "a1ed894e04510d07b4b6cf03267258357afdac6c7d824ab5d45428eda2d57a0a" "bf44f7d22051ac9478aa153ba50199ed0fbe359e1c1854fe7440dc000a6e1228" "cccd52d7943e895c3c850d9f49298872cb57c51f5b73349ef3b2bc3283b9f3f5" "362d2dc4e0fa755e0c1009ed0c37a206c92dc7bda0ded39cec8df32d13a98b0a" "c8b27c432c3983f4e703c592327bf0d10ff890e3054d47ee471c5f7d4dc5aef8" "65f8d6345f5898b1ee86b885fd61e2ce85f3589722e0cabe8df7cb4daa3e88b5" "81d75a80856e7368952dd3408c6cdb6935f1d6199579d372ae9004b7307bbe4b" "736e0868e57d6e29f3b1cc29c8edc7b62692ae4e1dd1429f9ecd934ba0f8a697" "5d97cce1063d40c95eda54bb69eb42ed05e839313d181c49464679e38285232d" "5d92dcbb0ceb8fe059732fce6d433c77ec3f8b439e9e2f6bbfb947df89b89979" "f51a4d4328c4a235942d819cd71f6b0c761296bc2cfc2cde44f33bbfc0711e05" "df61a8a15e9aac1f9925c452adfa532ec14f07ab40d7097617a0e7cbb4456fb6" "f89abc033d8b27200f38ecb7e3dd9b9ae6100211cdd7e489c1e86318dbaa0a9e" "537cfac1c3b28b04bc41341abb5857f9549905718abb540fe67accd7b3bdb73f" "ad6d43807539295bb05aab6577b41776deaff984aede94432961b2de59265abf" default)))
 '(org-agenda-files (quote ("~/todo.org" "~/org/ytt.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
