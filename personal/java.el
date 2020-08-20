;;;; ============================================================+
;;;; JAVA Development Environment                                |
;;;; ------------------------------------------------------------+
;;;;
;;;; This is a java specific configuration. Java stands out as it is
;;;; the programming language that needs the IDE the most. Especially
;;;; if you want to do Android development as well.
;;;;
;;;; This configuration is centered on lsp-java.



;; NOTE: the first when a .java file is opened, you will be prompted
;; to install JDT Language Server (jdtls). Just type `jdtls` and it
;; will go smoothly.
(use-package lsp-java :ensure t
  :config (progn
            (add-hook 'java-mode-hook #'lsp)))

;; TODO(breakds): Consider adding projectile if needed.

;;; +============================================================+
;;; | LSP (Language Server Protocol) Support                     |
;;; +------------------------------------------------------------+
;;;
;;; There are two different flavors of lsp client for emacs.
;;; 1) lsp-mode
;;; 2) eglot
;;;
;;; The argument on github: https://github.com/joaotavora/eglot/issues/180
;;;
;;; Below is the configuration for `lsp-mode`.
;;;
;;; Note: I am using this with clangd as lsp server for C++ (YMMV).

(use-package yasnippet :ensure t)

(use-package flycheck :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :config (progn
            (add-hook 'after-init-hook 'global-company-mode)
            (setq company-idle-delay 0)))

(use-package company-lsp :ensure t)

(use-package lsp-ui :ensure t)

(use-package lsp-mode
  :ensure t
  ;; Auto-enabled for c++ major mode
  :hook ((c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init (progn
          (setq lsp-auto-guess-root t)
          ;; Automatically configure company-lsp and lsp-ui
          (setq lsp-auto-configure t))
  :config (progn
            (push 'company-lsp company-backends)))

;; All credits to https://github.com/emacs-lsp/lsp-java
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
;; (use-package helm-lsp :ensure t)
;; (use-package helm
;;   :ensure t
;;   :config (helm-mode))
(use-package treemacs :ensure t)
(use-package lsp-treemacs :ensure t)


