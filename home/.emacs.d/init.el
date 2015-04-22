;;; init -- Mike's init.el file

;; Author: Mike Charlton

;;; Commentary:

;; Basically trying to keep this as simple as possible.  The intent is to provide
;; the simplest thing that will get me able to use Evil mode in roughly
;; the same way that I use Vim.

;;; Code:

;; disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; turn off menu bar
(menu-bar-mode -1)

;; set indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'ruby-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'coffee-tab-width 'tab-width)

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
(require-package 'evil)
(eval-after-load 'evil-ex
  '(evil-ex-define-cmd "b[wipeout]" 'kill-buffer))

(require-package 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(require-package 'coffee-mode)

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Turn on Evil mode
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (agnostic)))
 '(custom-safe-themes
   (quote
    ("f51a4d4328c4a235942d819cd71f6b0c761296bc2cfc2cde44f33bbfc0711e05" "df61a8a15e9aac1f9925c452adfa532ec14f07ab40d7097617a0e7cbb4456fb6" "f89abc033d8b27200f38ecb7e3dd9b9ae6100211cdd7e489c1e86318dbaa0a9e" "537cfac1c3b28b04bc41341abb5857f9549905718abb540fe67accd7b3bdb73f" "ad6d43807539295bb05aab6577b41776deaff984aede94432961b2de59265abf" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
