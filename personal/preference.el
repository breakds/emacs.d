;;;; ============================================================+
;;;; Minor preferences                                           |
;;;; ------------------------------------------------------------+

;;; +============================================================+
;;; | Key Bindings                                               |
;;; + -----------------------------------------------------------+
;;;
;;; * M-p
;;;   Delete trailing whitespaces.
;;;
;;; * C-c M-d
;;;   Insert the current date.

;;; Remove the trailing white spaces to tidy the code/article.
(global-set-key (kbd "M-p") 'delete-trailing-whitespace)

(setq browse-url-generic-program "google-chrome")
(setq browse-url-browser-function 'browse-url-generic)

;;; Insert time stamp
(defun bds/insert-current-timestamp ()
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(global-set-key (kbd "C-c M-d") 'bds/insert-current-timestamp)

;;; Helper Functions

(defun bds/gc-buffers (path-pattern)
  "Kill all the buffers whose file name path matches the input pattern."
  (interactive "sEnter the path pattern: ")
  (loop for buf in (buffer-list)
        for path = (buffer-file-name buf)
        when (and path
                  (string-match-p path-pattern path))
        do (kill-buffer buf)))
