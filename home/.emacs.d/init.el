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

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
                    '(progn ,@body)))

(use-package magit
             :ensure t
             :config
             (progn
               (setq magit-last-seen-setup-instructions "1.4.0")

               (after 'evil
                      ;; Yes, this is necessary to get Vim keybindings in Magit for the moment
                      ;; See @uu1101's post in https://github.com/magit/magit/issues/1968
                      (dolist (map (list
                                     ;; Mode maps
                                     ;; magit-blame-mode-map
                                     magit-cherry-mode-map
                                     magit-diff-mode-map
                                     magit-log-mode-map
                                     ;; magit-log-select-mode-map
                                     magit-mode-map
                                     ;; No evil keys for the popup.
                                     ;; magit-popup-help-mode-map
                                     ;; magit-popup-mode-map
                                     ;; magit-popup-sequence-mode-map
                                     magit-process-mode-map
                                     magit-reflog-mode-map
                                     ;; magit-refs-mode-map
                                     ;; magit-revision-mode-map
                                     ;; magit-stash-mode-map
                                     ;; magit-stashes-mode-map
                                     magit-status-mode-map
                                     ;; Section submaps
                                     ;; magit-branch-section-map
                                     ;; magit-commit-section-map
                                     ;; magit-file-section-map
                                     ;; magit-hunk-section-map
                                     ;; magit-module-commit-section-map
                                     ;; magit-remote-section-map
                                     ;; magit-staged-section-map
                                     ;; magit-stash-section-map
                                     ;; magit-stashes-section-map
                                     ;; magit-tag-section-map
                                     ;; magit-unpulled-section-map
                                     ;; magit-unpushed-section-map
                                     ;; magit-unstaged-section-map
                                     ;; magit-untracked-section-map
                                     ))
                        ;; Move current bindings for movement keys to their upper-case counterparts.
                        (dolist (key (list "k" "j" "h" "l"))
                          (let ((binding (lookup-key map key)))
                            (when binding
                              (define-key map (upcase key) binding) (define-key map key nil))))
                        (evil-add-hjkl-bindings map 'emacs
                                                (kbd "v") 'evil-visual-char
                                                (kbd "V") 'evil-visual-line
                                                (kbd "C-v") 'evil-visual-block
                                                (kbd "C-w") 'evil-window-map))
                      (dolist (mode (list 'magit-blame-mode
                                          'magit-cherry-mode
                                          'magit-diff-mode
                                          'magit-log-mode
                                          'magit-log-select-mode
                                          'magit-mode
                                          'magit-popup-help-mode
                                          'magit-popup-mode
                                          'magit-popup-sequence-mode
                                          'magit-process-mode
                                          'magit-reflog-mode
                                          'magit-refs-mode
                                          'magit-revision-mode
                                          'magit-stash-mode
                                          'magit-stashes-mode
                                          'magit-status-mode))
                        (add-to-list 'evil-emacs-state-modes mode))
                      ;; end vim keybinding massive hack

                      (define-key evil-normal-state-map (kbd ", g") 'magit-status))))

(require-package 'coffee-mode)

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Turn on Evil mode
(evil-mode 1)

;; Set up some global keybindings for Org mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/ytt.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (agnostic)))
 '(custom-safe-themes
   (quote
    ("362d2dc4e0fa755e0c1009ed0c37a206c92dc7bda0ded39cec8df32d13a98b0a" "c8b27c432c3983f4e703c592327bf0d10ff890e3054d47ee471c5f7d4dc5aef8" "65f8d6345f5898b1ee86b885fd61e2ce85f3589722e0cabe8df7cb4daa3e88b5" "81d75a80856e7368952dd3408c6cdb6935f1d6199579d372ae9004b7307bbe4b" "736e0868e57d6e29f3b1cc29c8edc7b62692ae4e1dd1429f9ecd934ba0f8a697" "5d97cce1063d40c95eda54bb69eb42ed05e839313d181c49464679e38285232d" "5d92dcbb0ceb8fe059732fce6d433c77ec3f8b439e9e2f6bbfb947df89b89979" "f51a4d4328c4a235942d819cd71f6b0c761296bc2cfc2cde44f33bbfc0711e05" "df61a8a15e9aac1f9925c452adfa532ec14f07ab40d7097617a0e7cbb4456fb6" "f89abc033d8b27200f38ecb7e3dd9b9ae6100211cdd7e489c1e86318dbaa0a9e" "537cfac1c3b28b04bc41341abb5857f9549905718abb540fe67accd7b3bdb73f" "ad6d43807539295bb05aab6577b41776deaff984aede94432961b2de59265abf" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
