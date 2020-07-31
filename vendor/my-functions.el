;; Generic useful functions

(defun collapse-whitespace-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;; For killing the Emacs daemon safely -- code taken from
;; http://www.emacswiki.org/emacs/EmacsAsDaemon

(defun client-save-kill-emacs(&optional display)
  "This is a function that can bu used to shutdown save buffers and shut down
the emacs daemon. It should be called using
  emacsclient -e '(client-save-kill-emacs)'
This function will check to see if there are any modified buffers or active
clients or frame.  If so an x window will be opened and the user will be
prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames
          (or (> (length server-clients) 1)
              (> (length (frame-list)) 1)))

    ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
        (x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ; Save the current frame.
    (setq new-frame (selected-frame))


    ; When displaying the number of clients and frames:
    ; subtract 1 from the clients for this client.
    ; subtract 2 from the frames for this frame (that we just created)
    ; and the default frame.
    (when
        (or (not active-clients-or-frames)
            (yes-or-no-p
             (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

      ; If the user quits during the save dialog then don't exit emacs.
      ; Still close the terminal though.
      (let((inhibit-quit t))
                                        ; Save buffers
        (with-local-quit
          (save-some-buffers))

        (if quit-flag
            (setq quit-flag nil)
                                        ; Kill all remaining clients
          (progn
            (dolist (client server-clients)
              (server-delete-client client))
                                        ; Exit emacs
            (kill-emacs)))
        ))

    ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames)
      (delete-frame new-frame)))
  )


(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when
          (and (buffer-live-p buffer)
               (buffer-modified-p buffer)
               (not (buffer-base-buffer buffer))
               (or
                (buffer-file-name buffer)
                (progn
                  (set-buffer buffer)
                  (and buffer-offer-save (> (buffer-size) 0)))))
        (setq modified-found t)))
    modified-found))


;; incremental nums
(defun inc-num-region (p m)
  "Increments the numbers in a given region"
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region p m)    
      (goto-char (point-min))   
      (forward-line)
      (let ((counter 1))
        (while (not (eq (point)
                        (point-max)))
          (goto-char (point-at-eol))
          (search-backward-regexp "[0-9]+" (point-at-bol) t)
          (let* ((this-num (string-to-number (match-string 0)))
                 (new-num-str (number-to-string (+ this-num
                                                   counter))))
            (replace-match new-num-str)
            (incf counter)
            (forward-line)))))))


;; add code review note
(defun add-code-review-note ()
  "Add note for current file and line number"
  (interactive)
  (let ((file-name (buffer-file-name))
        (file-line (line-number-at-pos)))
    (switch-to-buffer-other-window (get-buffer-create "NOTES"))
    (goto-char (point-min))
    (when (not (search-forward "-*- mode:compilation-shell-minor"
                               nil t))
      (compilation-shell-minor-mode 1)
      (insert "-*- mode:compilation-shell-minor -*-\n\n"))
    (goto-char (point-max))
    (if (/= (current-column) 0)
        (newline))
    (insert file-name ":" (number-to-string file-line) ": ")))


;; automatically add ifndef...define for C header
(defun get-include-guard ()
   "Return a string suitable for use in a C/C++ include guard"
   (let* ((fname (buffer-file-name (current-buffer)))
          (fbasename (replace-regexp-in-string ".*/" "" fname))
          (inc-guard-base (replace-regexp-in-string "[.-]"
                                                    "_"
                                                    fbasename)))
     (upcase inc-guard-base)))
 
 (add-hook 'find-file-not-found-hooks
           '(lambda ()
              (let ((file-name (buffer-file-name (current-buffer))))
                (when (string= ".h" (substring file-name -2))
                  (let ((include-guard (get-include-guard)))
                    (insert "#ifndef " include-guard)
                    (newline)
                    (insert "#define " include-guard)
                    (newline 4)
                    (insert "#endif")
                    (newline)
                    (previous-line 3)
                    (set-buffer-modified-p nil))))))
