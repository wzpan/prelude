(provide 'traditional-to-simplified)

(defun traditional-to-simplified (arg) 
  "简体中文转换为繁体中文. 
加参数为繁体中文转换为简体中文." 
  (interactive "P") 
  (let (from to buffer char point) 
    (if arg  
        (setq arg (car arg)) 
      (setq arg 0)) 

    (if (= arg 0) 
        (progn 
          (setq from "~/traditional") 
          (setq to "~/simplified"))
      (progn
        (setq from "~/simplified") 
        (setq to "~/traditional"))) 
    (setq buffer (buffer-name)) 

    (find-file from) 
    (find-file to) 
    (setq point 0) 

    (set-buffer buffer) 
    (goto-char (point-min)) 
    (while (re-search-forward "\\(\\cc\\)" nil t) 
      (setq char (match-string-no-properties 1)) 
      ;; 在 from 文件中找 
      (save-match-data 
        ;; open file and save a handle to the buffer 
        (set-buffer (get-file-buffer from)) 
        (goto-char (point-min)) 
        (if (re-search-forward char nil t) 
            (setq point (match-beginning 0))) 
        (bury-buffer) 
        (if (> point 0) 
            (progn  
              ;; open file and save a handle to the buffer 
              (set-buffer (get-file-buffer to)) 
              (setq char (buffer-substring point (+ point 1))) 
              (bury-buffer)))) 
      (set-buffer buffer) 
      (replace-match char) 
      (setq point 0)) 
    (kill-buffer (get-file-buffer from)) 
    (kill-buffer (get-file-buffer to)))) 
