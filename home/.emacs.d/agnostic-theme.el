;;; agnostic --- A 16 color theme for sharing sessions over tmux

;;; Commentary:
;;; Please see https://github.com/ygt-mikekchar/agnostic

;;; Code:
(deftheme agnostic
  "Created 2015-04-22.")

(custom-theme-set-faces
 'agnostic
 ;;;; basic faces
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "brightwhite" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(cursor ((t (:background "brightred"))))
 '(error  ((t (:foreground "brightred" :weight bold))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "brightred" :weight bold))))
 '(minibuffer-prompt ((t (:background "magenta" :foreground "black"))))
 '(highlight ((t (:background "brightblack"))))
 '(region ((t (:background "brightblack"))))
 '(shadow ((t (:foreground "brightwhite"))))
 '(secondary-selection ((t (:background "cyan" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:foreground "brightred" :weight bold))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "brightblue"))))
 '(font-lock-constant-face ((t (:foreground "brightcyan" :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "brightred" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "brightmagenta" :weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "brightcyan"))))
 '(font-lock-type-face ((t (:foreground "brightyellow"))))
 '(font-lock-variable-name-face ((t (:foreground "brightblue"))))
 '(font-lock-warning-face ((t (:background "yellow" :foreground "brightwhite" :weight bold))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "brightmagenta" :underline t))))
 '(link-visited ((t (:inherit link :foreground "brightblue"))))
 '(fringe ((t (:background "brightblack"))))
 '(header-line ((t (:background "cyan" :foreground "brightwhite" :weight bold))))
 '(tooltip ((t (:inherit variable-pitch :background "yellow" :foreground "brightwhite"))))

 ;;;; search colours
 '(isearch ((t (:background "yellow" :foreground "brightwhite"))))
 '(isearch-fail ((t (:background "red"))))
 '(lazy-highlight ((t (:background "cyan"))))
 '(match ((t (:background "cyan" :foreground "white"))))
 '(next-error ((t (:inherit region))))
 '(query-replace ((t (:inherit isearch))))

 ;;;; modeline and border colours
 '(mode-line ((t (:background "white" :foreground "blue" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:background "brightblue" :foreground "brightblack" :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "brightwhite" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "white" :foreground "brightblack" :box (:line-width -1 :color "brightwhite") :weight light))))
 '(linum ((t (:background "white" :foreground "brightblack"))))

 ;;;; popup colours
 '(popup-scroll-bar-background-face ((t (:background "white"))))
 '(popup-scroll-bar-foreground-face ((t (:background "brightred"))))
 '(popup-tip-face ((default (:inherit (tooltip)))))
 '(popup-face ((t (:foreground "brightwhite" :background "blue"))))
 '(popup-isearch-match ((t (:background "cyan"))))
 '(popup-menu-face ((default (:inherit (tooltip)))))
 '(popup-menu-mouse-face ((t (:background "magenta"))))
 '(popup-menu-selection-face ((t (:background "blue"))))
 '(popup-menu-summary-face ((t (:foreground "brightmagenta"))))
 '(popup-summary-face ((t (:foreground "brightred" :background "blue"))))

 ;;;; diff colours
 '(diff-added ((t (:background "green"))))
 '(diff-removed ((t (:background "red"))))
 '(diff-changed ((t (:background "blue"))))

 ;;;; ediff colours
 '(ediff-current-diff-A ((t (:background "blue"))))
 '(ediff-fine-diff-A ((t (:background "yellow"))))
 '(ediff-even-diff-A ((t (:background "green"))))
 '(ediff-odd-diff-A ((t (:background "red"))))
 '(ediff-current-diff-B ((t (:background "brightblue"))))
 '(ediff-fine-diff-B ((t (:background "brightyellow"))))
 '(ediff-even-diff-B ((t (:background "brightgreen"))))
 '(ediff-odd-diff-B ((t (:background "brightred"))))

 ;;;; Org mode colours
 '(org-hide ((t (:inherit (font-lock-comment-face)))))
 '(org-level-1 ((t (:foreground "brightyellow" :weight bold))))
 '(org-level-2 ((t (:foreground "brightyellow"))))
 '(org-level-3 ((t (:foreground "brightyellow"))))
 '(org-level-4 ((t (:foreground "brightyellow"))))
 '(org-level-5 ((t (:foreground "brightyellow"))))
 '(org-level-6 ((t (:foreground "brightyellow"))))
 '(org-level-7 ((t (:foreground "brightyellow"))))
 '(org-date ((t (:foreground "brightmagenta"))))
 '(org-footnote  ((t (:foreground "brightmagenta"))))
 '(org-link ((t (:inherit (link)))))
 '(org-special-keyword ((t (:foreground "brightred"))))
 '(org-verbatim ((t (:foreground "brightmagenta"))))
 '(org-block ((t (:foreground "brightgreen"))))
 '(org-quote ((t (:inherit org-block))))
 '(org-verse ((t (:inherit org-block))))
 '(org-todo ((t (:foreground "brightred" :weight bold))))
 '(org-done ((t (:foreground "brightgreen" :weight bold))))
 '(org-agenda-structure ((t (:weight bold :foreground "brightred"))))
 '(org-agenda-date ((t (:inherit (org-date)))))
 '(org-agenda-date-weekend ((t (:weight normal :foreground "brightblue"))))
 '(org-agenda-date-today ((t (:weight bold :foreground "brightyellow"))))
 '(org-table ((t (:foreground "brightblue"))))
 '(org-time-grid ((t (:foreground "brightyellow"))))
 '(org-agenda-restriction-lock ((t (:background "blue" :foreground "white"))))
 '(org-document-info ((t (:foreground "brightcyan"))))
 '(org-document-title ((t (:weight bold :foreground "brightcyan"))))
 '(org-ellipsis ((t (:foreground "brightyellow"))))

 ;;;;; magit
 ;;;;;; headings and diffs
 '(magit-section-highlight           ((t (:background "black"))))
 '(magit-section-heading             ((t (:foreground "brightyellow" :weight bold))))
 '(magit-section-heading-selection   ((t (:foreground "brightred" :weight bold))))
 '(magit-diff-file-heading           ((t (:weight bold))))
 '(magit-diff-file-heading-highlight ((t (:background "black" :weight bold))))
 '(magit-diff-file-heading-selection ((t (:background "black"
                                                      :foreground "brightred" :weight bold))))
 '(magit-diff-hunk-heading
   ((t (:background "yellow"))))
 '(magit-diff-hunk-heading-highlight
   ((t (:background "yellow"))))
 '(magit-diff-hunk-heading-selection
   ((t (:background "yellow"
                    :foreground "brightred"
                    :weight bold))))
 '(magit-diff-lines-heading          ((t (:background "red"
                                                      :foreground "brightwhite"))))
 '(magit-diff-context-highlight      ((t (:background "black"))))
 '(magit-diffstat-added              ((t (:foreground "brightgreen"))))
 '(magit-diffstat-removed            ((t (:foreground "brightred"))))

 ;;;;;; popup
 '(magit-popup-heading             ((t (:foreground "brightcyan" :weight normal))))
 '(magit-popup-key                 ((t (:foreground "brightcyan" :weight bold))))
 '(magit-popup-argument            ((t (:foreground "brightcyan" :weight bold))))
 '(magit-popup-disabled-argument   ((t (:foreground "brightgreen" :weight normal))))
 '(magit-popup-option-value        ((t (:foreground "brightcyan" :weight bold))))

 ;;;;;; process
 '(magit-process-ok    ((t (:foreground "brightgreen" :weight bold))))
 '(magit-process-ng    ((t (:foreground "brightred"   :weight bold))))

 ;;;;;; log
 '(magit-log-author    ((t (:foreground "brightgreen" :weight bold))))
 '(magit-log-date      ((t (:foreground "brightgreen"))))
 '(magit-log-graph     ((t (:foreground "brightcyan"))))

 ;;;;;; sequence
 '(magit-sequence-pick ((t (:foreground "brightyellow"))))
 '(magit-sequence-stop ((t (:foreground "brightgreen"))))
 '(magit-sequence-part ((t (:foreground "brightyellow"))))
 '(magit-sequence-head ((t (:foreground "brightblue"))))
 '(magit-sequence-drop ((t (:foreground "brightred"))))
 '(magit-sequence-done ((t (:foreground "brightgreen"))))
 '(magit-sequence-onto ((t (:foreground "brightgreen"))))

 ;;;;;; bisect
 '(magit-bisect-good ((t (:foreground "brightgreen"))))
 '(magit-bisect-skip ((t (:foreground "brightyellow"))))
 '(magit-bisect-bad  ((t (:foreground "brightred"))))

 ;;;;;; blame
 '(magit-blame-heading ((t (:background "black" :foreground "brightmagenta"
                                        :weight bold :slant normal :box (:color "black" :line-width 2)))))
 '(magit-blame-hash    ((t (:background "black" :foreground "brightmagenta"
                                        :weight normal :slant normal :box (:color "black" :line-width 2)))))
 '(magit-blame-name    ((t (:background "black" :foreground "brightmagenta"
                                        :weight normal :slant normal :box (:color "black" :line-width 2)))))
 '(magit-blame-date    ((t (:background "black" :foreground "brightmagenta"
                                        :weight bold :slant normal :box (:color "black" :line-width 2)))))
 '(magit-blame-summary ((t (:background "black" :foreground "brightblue"
                                        :weight bold :slant normal :box (:color "black" :line-width 2)))))

 ;;;;;; references etc.
 '(magit-dimmed         ((t (:foreground "brightgreen"))))
 '(magit-hash           ((t (:foreground "brightgreen"))))
 '(magit-tag            ((t (:foreground "brightcyan" :weight bold))))
 '(magit-branch-remote  ((t (:foreground "brightgreen"  :weight bold))))
 '(magit-branch-local   ((t (:foreground "brightblue"   :weight bold))))
 '(magit-branch-current ((t (:foreground "brightblue"   :weight bold :box t))))
 '(magit-head           ((t (:foreground "brightblue"   :weight bold))))
 '(magit-refname        ((t (:background "black" :foreground "brightgreen" :weight bold))))
 '(magit-refname-stash  ((t (:background "black" :foreground "brightgreen" :weight bold))))
 '(magit-refname-wip    ((t (:background "black" :foreground "brightgreen" :weight bold))))
 '(magit-signature-good      ((t (:foreground "brightgreen"))))
 '(magit-signature-bad       ((t (:foreground "brightred"))))
 '(magit-signature-untrusted ((t (:foreground "brightyellow"))))
 '(magit-cherry-unmatched    ((t (:foreground "brightyellow"))))
 '(magit-cherry-equivalent   ((t (:foreground "brightmagenta"))))
 '(magit-reflog-commit       ((t (:foreground "brightgreen"))))
 '(magit-reflog-amend        ((t (:foreground "brightmagenta"))))
 '(magit-reflog-merge        ((t (:foreground "brightgreen"))))
 '(magit-reflog-checkout     ((t (:foreground "brightblue"))))
 '(magit-reflog-reset        ((t (:foreground "brightred"))))
 '(magit-reflog-rebase       ((t (:foreground "brightmagenta"))))
 '(magit-reflog-cherry-pick  ((t (:foreground "brightgreen"))))
 '(magit-reflog-remote       ((t (:foreground "brightcyan"))))
 '(magit-reflog-other        ((t (:foreground "brightcyan"))))

 ;;; flycheck
 '(flycheck-error            ((t (:background "red" :weight bold))))
 '(flycheck-warning          ((t (:background "yellow" :foreground "brightblue"))))
 '(flycheck-info             ((t (:background "blue" :foreground "brightyellow"))))
 '(flycheck-fringe-error     ((t (:background "yellow" :foreground "brightred"))))
 '(flycheck-fringe-warning   ((t (:background "yellow" :foreground "brightblue"))))
 '(flycheck-fringe-info      ((t (:background "yellow" :foreground "brightgreen"))))
 '(flycheck-error-list-error ((t (:background "red" :weight bold))))
 '(flycheck-error-list-warning ((t (:background "yellow" :weight bold))))
 '(flycheck-error-list-info  ((t (:background "green" :weight bold))))

 ;;; Company-mode
 '(company-scrollbar-bg         ((default (:inherit (popup-scroll-bar-background-face)))))
 '(company-scrollbar-fg         ((default (:inherit (popup-scroll-bar-foreground-face)))))
 '(company-tooltip              ((default (:inherit (tooltip)))))
 '(company-tooltip-annotation   ((t (:foreground "brightred"))))
 '(company-tooltip-common       ((default (:inherit (popup-menu-summary-face)))))
 '(company-tooltip-selection    ((default (:inherit (popup-menu-selection-face)))))
 '(company-echo-common          ((t (:foreground "brightred"))))
 '(company-preview              ((default (:inherit (popup-face)))))
 '(company-preview-common       ((default (:inherit (popup-summary-face)))))
 '(company-preview-search       ((default (:inherit (popup-isearch-match)))))
 )

(provide-theme 'agnostic)
;;; agnostic-theme.el ends here
