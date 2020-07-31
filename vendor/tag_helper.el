;; some settings to help with my blogging

(defun mfa/extract-file-name (file-name)
  (file-name-nondirectory (file-name-sans-extension file-name)))
(defun mfa/get-directory-from-bufname (file-name)
  (mfa/extract-file-name file-name))
(defun mfa/get-images-directory (file-name)
  (concat (file-name-as-directory (expand-file-name "" "../images/"))
          (mfa/get-directory-from-bufname file-name)))
(defun mfa/list-image-files (directory)
  (mapcar 'file-name-nondirectory
          (directory-files directory
                           :match-regexp "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)")))
(defun mfa/yield-choices (file-name)
  (mfa/list-image-files (mfa/get-images-directory file-name)))

;; some settings to help with latex
(defun mfa/get-figures-directory (file-name)
  (file-name-as-directory (expand-file-name "" "./figures/")))
(defun mfa/yield-figure-choices (file-name)
  (mfa/list-image-files (mfa/get-figures-directory file-name)))

(defun my-screenshot ()
  "Take a screenshot into a unique-named file in the current buffer file
  directory and insert a link to this file."
  (interactive)
  (setq dirname
		(mfa/get-images-directory (buffer-file-name)))
  (setq absolutelocation
		(concat (make-temp-name
				 (concat  dirname "/" ) ) ".png"))
  (setq filename
		(concat (mfa/extract-file-name absolutelocation) ".png"))
  (setq relatedlocation
		(concat "/images/" (mfa/extract-file-name (buffer-file-name)) "/" filename))
  (if (not (file-directory-p dirname))
	  (call-process-shell-command "mkdir" nil nil nil nil nil dirname)
	  )
  ;(suspend-frame)
  (call-process-shell-command "scrot" nil nil nil nil " -s " (concat
															  "\"" absolutelocation "\"" ))
  (insert (concat "![](" relatedlocation ")"))
  )

(global-set-key (kbd "C-z p") 'my-screenshot)

(provide 'tag_helper)
