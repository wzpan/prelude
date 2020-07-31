;;; EMMS

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(require 'emms-score)
(emms-score 1)
;; autodetect musci files id3 tags encodeing
(require 'emms-i18n)
;; auto-save and import last playlist
(require 'emms-history)

;; format current track,only display title in mode line
(defun bigclean-emms-mode-line-playlist-current ()
  "Return a description of the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond ((and title)
                   title)))))
(setq emms-mode-line-mode-line-function
      'bigclean-emms-mode-line-playlist-current)

;; Show the current track each time EMMS
;; starts to play a track with "正在播放 : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "正在播放: %s")

;; When asked for emms-play-directory,
;; always start from this one
(setq emms-source-file-default-directory "~/Music/")

;; Want to use alsa with mpg321 ?
(setq emms-player-mpg321-parameters '("-o" "alsa"))

;; Using libtag for reading tags
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

(provide 'my-emms)
