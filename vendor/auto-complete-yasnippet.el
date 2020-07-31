(require 'auto-complete)
(require 'auto-complete-config)

(require 'yasnippet)

(defun ac-yasnippet-candidate ()
  (let ((table (yas/get-snippet-tables major-mode)))
    (if table
        (let (candidates (list))
          (mapcar (lambda (mode)          
                    (maphash (lambda (key value)    
                               (push key candidates))          
                             (yas/snippet-table-hash mode))) 
                  table)
          (all-completions ac-prefix candidates)))))

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white"))) 
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)) 
  "Source for Yasnippet.")

;; dirty fix for having AC everywhere
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                            (auto-complete-mode 1))
;;                        ))
;; (real-global-auto-complete-mode t)

(dolist (hook (list
              'emacs-lisp-mode-hook
              'lisp-mode-hook
              'lisp-interaction-mode-hook
               'scheme-mode-hook
               'c-mode-common-hook
               'haskell-mode-hook
               'asm-mode-hook
			   'python-mode-hook
               'emms-tag-editor-mode-hook
               'markdown-mode-hook
               ))
  (add-hook hook 'auto-complete-mode))

(dolist (hook (list
			   'emacs-lisp-mode-hook
			   'lisp-mode-hook
			   'lisp-interaction-mode-hook
               'scheme-mode-hook
               'c-mode-common-hook
               'haskell-mode-hook
               'asm-mode-hook
			   'python-mode-hook
               'emms-tag-editor-mode-hook
               'markdown-mode-hook
               ))
  (add-hook hook 'smartparens-mode))

;; auto-complete-clang
(require 'auto-complete-clang)
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/lib/gcc/x86_64-unknown-linux-gnu/
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include-fixed
 /usr/local/include
 /usr/include/c++/4.8.2/
 /usr/include/c++/4.8.2/x86_64-unknown-linux-gnu/
 /usr/include/c++/4.8.2/backward/
 /usr/include 
 /usr/include/qt/
 /usr/include/qt/QtCore/
 /usr/include/qt/QtGui/
 /usr/include/qt/QtWidgets/
 /usr/include/opencv/
 /usr/include/opencv2/
"
               )))

;; ;; ac-math
;; (require 'ac-math) 
;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

;;  (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
;;    (setq ac-sources
;;          (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;;                  ac-sources))
;;    )
;; (add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
;; (setq ac-math-unicode-in-math-p t)''

(provide 'auto-complete-yasnippet)
