;;;; ============================================================+
;;;; Global Settings and Personal Information                    |
;;;; ------------------------------------------------------------+

(setq user-full-name "Break Yang"
      user-mail-address "breakds@gmail.com")

;;; +============================================================+
;;; | Key Bindings                                               |
;;; + -----------------------------------------------------------+
;;;
;;; * C-x <Right> and C-x <Left>
;;;   winner-mode cycle windows.
;;;
;;; * C-x p
;;;   Go back to the mark (set by C-<space>).
;;;
;;; * C-x r
;;;   Open recent files.
;;;
;;; ------------------------------------------------------------
;;;
;;; * C-x u
;;;   Undo tree visualizer

;;; Diminish
;;; Hide certain major/minor mode in the mode line
;;; Can be used combined with `use-package` (Search for :dinimish below)
(use-package diminish :ensure t)

;;; Smart mode line
;;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line :ensure t)
(setq sml/theme 'respectful)
(sml/setup)

;;; Solarized Theme
(use-package color-theme-modern :ensure t)
(use-package color-theme-sanityinc-solarized :ensure t)
(load-theme 'sanityinc-solarized-light)

;;; Disable tab. Use space.
(setq-default indent-tabs-mode nil)

;;; Disable Toolbar.
(tool-bar-mode -1)

;;; Font
;;;
;;; If you really want to set it in your emacs configuration, use the
;;; one below. However, it is not portable and incompatible if you are
;;; sharing this eamcs configuration between displays of different
;;; resolutions (i.e. A normal monitor and a HiDpi one).
;;;
;;; The recommended way is to set "emacs.font" in ~/.Xresources
;;;
(set-frame-font "Fira Code-9")

;;; Change backup directory, and SAVE A LOT.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;; Make the kill ring larger
(setq kill-ring-max 5000)

;;; Save History
;;; https://www.wisdomandwonder.com/wp-content/uploads/2014/03/C3F.html
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;;; Time display in 24 hour format
(setq display-time-24hr-format t)
(display-time)

;;; Always highlight current line
(global-hl-line-mode t)

;;; Sentence should end with single space.
(setq sentence-end-double-space nil)

;;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;; Yes -> y, No -> n
(fset 'yes-or-no-p 'y-or-n-p)

;;; Getting back to the mark.
;;; Note that mark is set with C-<space>
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;;; Undo tree mode
;;; Visualize the undo/redo history and manipulation
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (progn
            (global-undo-tree-mode)
            (setq undo-tree-visualizer-timestamps t)
            (setq undo-tree-visualizer-diff t)))

;;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)
(bind-key "C-x r" 'recentf-open-files)

;;; Avoid huge jump when scrolling
(setq scroll-step 1) 
(defun scroll-up-slightly () (interactive) (scroll-up 3))
(defun scroll-down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'scroll-down-slightly)
(global-set-key [mouse-5] 'scroll-up-slightly)

;;; Use electric buffer list which is better than the default one
(global-set-key "\C-x\C-b" 'electric-buffer-list)
