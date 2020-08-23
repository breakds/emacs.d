;;;; ------------------------------------------------------------+
;;;; Emacs Configuration                                         |
;;;; ------------------------------------------------------------+
;;;; Author: Break Yang <breakds@gmail.com>
;;;;
;;;; References:
;;;; 1. Sacha Chua's emacs configuration (http://sach.ac/dotemacs)
;;;; 2. Eamcs as a C++ IDE (http://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html)
;;;; 3. sing Emacs as a C++ IDE (https://nilsdeppe.com/posts/emacs-c++-ide2)
;;;; 4. Angrybacon's emacs configuration (https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org)
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
;; ---------------------
;; Use straight.el to manage packages instead of package.el.
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


;; Load the module files
;; ---------------------
(defun bds-load-config-module (&rest path-segments)
  (org-babel-load-file (expand-file-name (string-join path-segments "/")
					 user-emacs-directory)))

(bds-load-config-module "modules" "basic.org")
(bds-load-config-module "modules" "appearance.org")
(garbage-collect)
