;;;; ============================================================+
;;;; Org Mode Configurations                                     |
;;;; ------------------------------------------------------------+
;;;;

;;; +============================================================+
;;; | Key Bindings                                               |
;;; + -----------------------------------------------------------+
;;;
;;; * C-c a
;;;   Org Agenda
;;;
;;; * C-c o
;;;   Main organizer file
;;;
;;; * C-c c
;;;   Capture
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c o") (lambda ()
                                (interactive)
                                (find-file "~/org/buffers.org")))
(global-set-key (kbd "C-c c") 'org-capture)

;;; +============================================================+
;;; | High-level Configurations                                  |
;;; + -----------------------------------------------------------+
(use-package org
  :config (setq org-startup-indented t))

(use-package ox-gfm :ensure t)

;;; TODO status and their styles
(progn
  (setq org-todo-keywords
        '((sequence "TODO(t)" "ONGOING(g)" "PAUSE(p)" "|"
                    "DONE(d)" "DELEGATED(l)" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
        '(("ONGOING" . (:foreground "blue" :weight bold))
          ("PAUSE" . (:foreground "red" :weight bold))
          ("DELEGATED" . (:foreground "orange" :weight bold)))))

;;; Paste links from outside
;;; Credit: https://pages.sachachua.com/.emacs.d/Sacha.html#orgfe5d909
(defun org/yank-link ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'org/yank-link)

;;; Save all org files every buffer.
(add-hook 'after-save-hook 'org-save-all-org-buffers)

;;; +============================================================+
;;; | Capture                                                    |
;;; + -----------------------------------------------------------+

;;; Where org-capture send the notes to
(setq org-default-notes-file "~/org/buffers.org")

;;; Templates
;;;
;;; TODO(breakds): Research on :clock-in and :clock-resume
(defun bds/habit-default-schedule ()
  (format-time-string "SCHEDULED: <%Y-%m-%d %a +7d>"))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/buffers.org" "Tasks")
         "* TODO %?\n")
        ("f" "favorite" entry (file+headline "~/org/buffers.org" "Favorite")
         "* %? :FAVORITE:\n%a\n")
        ("h" "habit" entry (file+headline "~/org/buffers.org" "Tasks")
         "* TODO %? :HABIT:\n%(bds/habit-default-schedule)\n:PROPERTIES:\n:STYLE: habit\n:END:\n")))

;;; +============================================================+
;;; | Agenda                                                     |
;;; + -----------------------------------------------------------+

(setq org-agenda-files
      (list "~/org/weride.org"
            "~/org/personal.org"
            "~/org/buffers.org"))

(setq org-agenda-span 10)
(setq org-agenda-start-on-weekday 0)

;;; +============================================================+
;;; | Refile                                                     |
;;; + -----------------------------------------------------------+

;;; Targets include this file and any agenda file, up to 3 levels.
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)
                           ("~/org/knowledge.org" :maxlevel . 2)))

;;; However, targets with DONE state are EXCLUDED as refile targets.
(setq org-refile-target-verify-function
      (lambda ()
        (not (member (nth 2 (org-heading-components))
                     org-done-keywords))))

;;; Refile to top-level is ALLOWED.
(setq org-refile-use-outline-path 'file)

;;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;; +============================================================+
;;; | Tags                                                       |
;;; + -----------------------------------------------------------+

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup . nil)
                      ("FAVORITE" . ?f)))

;;; +============================================================+
;;; | Habit                                                      |
;;; + -----------------------------------------------------------+

(setq org-habit-show-habits-only-for-today nil)

;;; +============================================================+
;;; | Archive                                                    |
;;; + -----------------------------------------------------------+

(defun bds/org-archive-all-done ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE/DELEGATED/CANCELLED" 'file))
