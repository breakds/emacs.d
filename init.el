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

;; Set up the load path for ELPA/MELPA packages.
;; - ELPA is the Emacs Lisp Package Archive
;; - MELPA is yet another repository
;; Note that Emacs users in China mainland can use '("popkit" . "http://elpa.popkit.org/packages/")
(require 'package)
(package-initialize nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; There seems to be a bug for Emacs 26.1 and 26.2, so that it fails
;; to download packages from "gnu" because of some tls certificate
;; issue. This fixes that by using 163.com mirror if needed.
;;
;; (setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          ("org" . "http://orgmode.org/elpa/")))

;; A dedicate place for emacs `customize` interface
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)

;; Packages for bootstrapping the configuration
(defmacro bootstrap-install (&rest packages)
  "This macro ensures that the input packages are all installed."
  `(let ((need-refresh nil))
     (loop for x in (list ,@packages)
	   when (not (package-installed-p x))
	   do (setf need-refresh t))
     (when need-refresh
       (package-refresh-contents)
       ,@(loop for x in packages
	       collect `(when (not (package-installed-p ,x))
			  (package-install ,x))))))

;; Install bootstrapping packages.
;; 1. use-package for loading packages and local directories.
(bootstrap-install 'use-package)

;; Set up use-package.
(require 'use-package)

;; Use load-dir to load the configuration directory which is set to
;; "~/.emacs.d/personal"
;; Ensure that the general configuration is loaded first.
(load "~/.emacs.d/general.el")
(use-package load-dir :ensure t)
(add-to-list 'load-dirs "~/.emacs.d/personal")
(load-dirs)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(TeX-view-program-list (quote (("Atril" "atril %o"))))
;;  '(TeX-view-program-selection
;;    (quote
;;     (((output-dvi style-pstricks)
;;       "dvips and gv")
;;      (output-dvi "xdvi")
;;      (output-pdf "Atril")
;;      (output-html "xdg-open"))))
;;  '(ledger-reports
;;    (quote
;;     (("2018" "ledger ")
;;      ("bal" "%(binary) -f %(ledger-file) bal")
;;      ("reg" "%(binary) -f %(ledger-file) reg")
;;      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
;;      ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
;;  '(markdown-command "grip --export -")
;;  '(org-agenda-files
;;    (quote
;;     ("~/org/cron.org" "~/org/reading.org" "~/org/unsorted.org" "~/org/projects.org")))
;;  '(package-selected-packages
;;    (quote
;;     (ledger-mode nix-mode dockerfile-mode json-mode tide web-mode-edit-element clang-format cmake-ide rtags flycheck company protobuf-mode yaml-mode web-mode use-package toc-org solarized-theme smart-mode-line org-doing org-dashboard markdown-mode+ load-dir forecast fcitx exec-path-from-shell elm-mode ein cargo))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
;;  '(package-selected-packages
;;    (quote
;;     (tabbar session pod-mode org muttrc-mode mutt-alias markdown-mode initsplit htmlize graphviz-dot-mode folding eproject diminish csv-mode browse-kill-ring boxquote bm bar-cursor apache-mode use-package load-dir))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
