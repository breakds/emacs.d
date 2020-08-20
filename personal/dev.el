;;;; ============================================================+
;;;; Development Environment                                     |
;;;; ------------------------------------------------------------+
;;;;
;;;; This includes the configuration for various languages.

;;; +============================================================+
;;; | C/C++                                                      |
;;; +------------------------------------------------------------+


;; Please treat .h as C++ code!
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Please respect C++11, C++14, C++17, C++20
(use-package modern-cpp-font-lock :ensure t)
(modern-c++-font-lock-global-mode t)

;; Global clang-format key
(use-package clang-format :ensure t)
(global-set-key [C-M-tab] 'clang-format-buffer-and-back-to-indentation)

(defun clang-format-buffer-and-back-to-indentation ()
  "Call clang-format to format the whole buffer, and move the
  cursor to the first non-space character of the current line."
    (interactive)
    (clang-format-buffer)
    (back-to-indentation))

(defun clang-format-bindings ()
  "Hijack the tab key to perform the function defined above,
  which is `clang-format-buffer-and-back-to-indentation`."
  (define-key c++-mode-map [tab]
    'clang-format-region))

(add-hook 'c++-mode-hook 'clang-format-bindings)

;;; +============================================================+
;;; | Python                                                     |
;;; +------------------------------------------------------------+

;;; Use 2-space tab to make code more compact
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;;; Add support for Bazel/Skylark files.
(add-to-list 'auto-mode-alist '("\\.BUILD" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE" . python-mode))

;;; Add support for jcon files.
(add-to-list 'auto-mode-alist '("\\.jcon" . python-mode))

;;; +============================================================+
;;; | Protocol Buffer                                            |
;;; +------------------------------------------------------------+

(use-package protobuf-mode :ensure t)

;;; +============================================================+
;;; | Rust                                                       |
;;; +------------------------------------------------------------+

(use-package rust-mode :ensure t)

;;; +============================================================+
;;; | Common Lisp                                                |
;;; +------------------------------------------------------------+

(use-package slime
  :ensure t
  :config (progn
            ;; Use this instead of sbcl because in NixOS quicklisp
            ;; wraps sbcl.
            (setq inferior-lisp-program "quicklisp run")
            (slime-setup '(slime-fancy))))

;;; +============================================================+
;;; | AucTeX                                                     |
;;; +------------------------------------------------------------+

(use-package tex
  :defer t
  :ensure auctex
  :config (setq TeX-auto-save t))

;;; +============================================================+
;;; | Javascript/Typescript                                      |
;;; +------------------------------------------------------------+

(use-package web-mode :ensure t)

(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))

;;; +============================================================+
;;; | ASM                                                        |
;;; +------------------------------------------------------------+

;;; TODO(breakds) Make this complete.

;;; +============================================================+
;;; | Git                                                        |
;;; +------------------------------------------------------------+

(use-package magit :ensure t)

(global-set-key (kbd "C-x g") 'magit-status)

;;; +============================================================+
;;; | Ledger                                                     |
;;; +------------------------------------------------------------+

(use-package ledger-mode :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.journal$" . ledger-mode))))

;;; +============================================================+
;;; | Nix                                                        |
;;; +------------------------------------------------------------+

(use-package nix-mode :ensure t)

;;; +============================================================+
;;; | CMake                                                      |
;;; +------------------------------------------------------------+

(use-package cmake-mode :ensure t)
