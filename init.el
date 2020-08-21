;;;; ------------------------------------------------------------+
;;;; Emacs Configuration                                         |
;;;; ------------------------------------------------------------+
;;;; Author: Break Yang <breakds@gmail.com>
;;;;
;;;; References:
;;;; 1. Sacha Chua's emacs configuration (http://sach.ac/dotemacs)
;;;; 2. Eamcs as a C++ IDE (http://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html)
;;;; 3. sing Emacs as a C++ IDE (https://nilsdeppe.com/posts/emacs-c++-ide2)
;;;;
;;;;
;;;; ---------- ELisp (Emacs Lisp) Notes ----------
;;;; 1. To get a REPL for Emacs Lisp, `M-x ielm`.

;;; +============================================================+
;;; | Key Bindings                                               |
;;; + -----------------------------------------------------------+
;;;
;;; * C-x <Right> and C-x <Left>
;;;   winner-mode cycle windows.

;;; +============================================================+
;;; | Basic Utilities for Emacs Configuration                    |
;;; +------------------------------------------------------------+

;; Enable Common Lisp
(require 'cl)

;; Bootstrap straight.el

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
					user-emacs-directory))
      (bootstrap-version 5)
      (straight-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously straight-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Solarized Theme
(straight-use-package 'color-theme-modern)
(straight-use-package 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-light t)

;; Diminish
;; --------
;; Hide certain major/minor mode in the mode line
;; Example Usage, see https://github.com/emacsmirror/diminish
(straight-use-package 'diminish)

;; el-patch
;; --------
;; Provides a way to customize the behavior of Emacs Lisp functions
;; that do not provide enough variables and hooks to let you
;; customize.
(straight-use-package 'el-patch)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
