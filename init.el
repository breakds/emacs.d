;;;; ------------------------------------------------------------+
;;;; Emacs Configuration                                         |
;;;; ------------------------------------------------------------+
;;;; Author: Break Yang <breakds@gmail.com>
;;;;
;;;; References:
;;;; 1. Sacha Chua's emacs configuration (http://sach.ac/dotemacs)
;;;; 2. Eamcs as a C++ IDE (http://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html)
;;;; 3. Using Emacs as a C++ IDE (https://nilsdeppe.com/posts/emacs-c++-ide2)
;;;; 4. Angrybacon's emacs configuration (https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org)
;;;; 5. Lupan's emacs configuration (https://lupan.pl/dotemacs/)
;;;; 6. rememberYou's emacs configuration (https://github.com/rememberYou/.emacs.d/blob/master/config.org)
;;;; 7. Turn your emacs.d into an emacs distribution with straight.el (https://countvajhula.com/2020/12/27/turn-your-emacs-d-into-an-emacs-distribution-with-straight-el/)
;;;; 8. Emacs from scratch (https://github.com/daviwil/emacs-from-scratch)
;;;;
;;;; ---------- ELisp (Emacs Lisp) Notes ----------
;;;; 1. To get a REPL for Emacs Lisp, `M-x ielm`.

;;; +============================================================+
;;; | Key Bindings                                               |
;;; + -----------------------------------------------------------+
;;;
;;; * C-x <Right> and C-x <Left>
;;;   winner-mode cycle windows.

;;; Increase the garbage collection threshold (by default it is 800KB, which is
;;; too conservative for a mordern computer) to 50MB. This will effectively
;;; reduce the number of gc performed during startup, and can increase the
;;; startup speed.
;;;
;;; On Samaritan with Ryzen 9 3900X, it reduces the startup time from 2.6
;;; seconds to 2.0 seconds.
(setq gc-cons-threshold (* 100 1000 1000))

;;; +============================================================+
;;; | Basic Utilities for Emacs Configuration                    |
;;; +------------------------------------------------------------+

;; Enable Common Lisp
(require 'cl-lib)

;; Bootstrap straight.el
;; ---------------------
;; Use straight.el to manage packages instead of package.el.
;;
;; Some notes about straight.el: straight.el is a package manager
;; drop-in replacement to package.el. When you write
;;
;; (use-package xxx :ensure t)
;;
;; It by defaults use package.el as the package manager backend to
;; install the package. However, you can delegate that to stright.el
;; by
;;
;; (use-package xxx :straight t) or (use-package xxx :straight (...))
;;
;; Where you put the detailed description of the package in ...
(setq package-enable-at-startup nil)  ;; This is to disable
				      ;; package.el, required for
				      ;; emacs version >= 27
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; About use-package
;; -----------------
;;
;; The good thing about use-package is that you get to control
;; precisely when a piece of configuration code will be executed via
;; something similar to lifecycle callbacks.
;;
;; The common callbacks are:
;;
;; 1. :init     execute code before a package is loaded
;; 2. :config   execute code after a package is loaded
;; 3. :bind     binds the key map but also implicitly defer the package
;; ...
(straight-use-package 'use-package)

;; Load the module files
;; ---------------------
(defun bds-load-config-module (&rest path-segments)
  (org-babel-load-file (expand-file-name (string-join path-segments "/")
					 user-emacs-directory)))

(bds-load-config-module "modules" "appearance.org")
(bds-load-config-module "modules" "completion.org")
;; (bds-load-config-module "modules" "ivy.org")
(bds-load-config-module "modules" "basic.org")
(bds-load-config-module "modules" "org-tweaks.org")
;; (bds-load-config-module "modules" "development.org")
(garbage-collect)

;; Now that startup is completed. Descreasing the GC threshold (i.e.
;; more frequently GC).
(setq gc-cons-threshold (* 20 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
