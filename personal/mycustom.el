(setq prelude-guru nil) ; activate arrow keys
;(setq prelude-fi nil) ;

(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-tomorrow/")
(require 'sanityinc-tomorrow-night-theme)
(load-theme 'sanityinc-tomorrow-night t)

; Setting English Font
(set-face-attribute 'default nil :font "Monaco 12")
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo)) (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Microsoft YaHei" :size 14)))

(setq make-backup-files nil)  ;do not generate backup file
(auto-image-file-mode t)  ;enable browsing image
(mouse-avoidance-mode 'animate)  ;avoid the arrow
;;(setq frame-title-format '("" buffer-file-name "    $HaHack$" ))  ;personalize buffer name
(setq kill-ring-max 200)  ;use a big kill ring

;;show clock
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;; mouse scrolling
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;; allow C-z as prefix key
(define-prefix-command 'ctl-z-map)
(global-set-key (kbd "C-z") 'ctl-z-map)
;;(global-set-key (kbd "C-k") 'kill-line)

;; customize keys
(global-set-key [f1] 'shell);enter Shell
(global-set-key (kbd "C-<f1>") 'create-shell) ;new shell
(global-set-key [f3] 'repeat-complex-command) ;repeat command
(setq compile-command "make -f Makefile")
(global-set-key [f7] 'do-compile);;  do compile
(global-set-key (kbd "C-x 】") 'forward-page)
(global-set-key (kbd "C-x 【") 'backward-page)
(global-set-key (kbd "C-z d") 'hungry-delete-forward)
(global-set-key (kbd "C-z b") 'hungry-delete-backward)
(global-set-key (kbd "C-z o") 'previous-multiframe-window)
(global-set-key (kbd "M-<delete>") 'kill-word)

;; default shell
(setq explicit-shell-file-name "/bin/bash")

;; C-S-i to open setup file
(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/personal/mycustom.el"))
(global-set-key (kbd "C-S-i") 'open-init-file)

;; dired setup
;; avoid creating new buffer each time accessing a new folder
(defadvice dired-find-file (around dired-find-file-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-file-for-visit)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))
(defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer)))
    ad-do-it
    (kill-buffer orig)))


;; undo & redo
(defalias 'redo 'undo-tree-redo)

;; tabbar
(require 'tabbar)
(tabbar-mode)
(global-linum-mode)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(load "desktop")
(desktop-save-mode)

;; ispell
;; (setq ispell-dictionary "english")

;; personal info
(setq user-mail-address "panweizhou500@pingan.com.cn")
(setq user-full-name "panweizhou500")

;; graphviz
(autoload 'graphviz-dot-mode "graphviz-dot-mode.el" "graphviz dot mode." t)

;; markdown-mode
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)
;; disable <tab>
(defun markdown-unset-tab ()
 "markdown-mode-hook"
 (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))

(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook #'LaTeX-install-toolbar)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; use okular to view pdf files
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list '(("PDF Viewer" "okular %o")))

;; use xelatex as the default latex compiler
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex -file-line-error %(mode)%' %t" TeX-run-TeX nil t))
            (setq fill-column 80)
            (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query  nil )
            (setq TeX-show-compilation t)
            ))

;; hignlight a symbol
(require 'highlight-symbol)
(global-set-key [f4] 'highlight-symbol-at-point)
(global-set-key [C-f4] 'highlight-symbol-next)
(global-set-key [M-f4] 'highlight-symbol-prev)

;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
      '(
        ;; (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

;;; advice for whitespace-mode conflict
(defvar my-prev-whitespace-mode nil)
(make-variable-buffer-local 'my-prev-whitespace-mode)

(defadvice popup-draw (before my-turn-off-whitespace)
  "Turn off whitespace mode before showing autocomplete box"
  (make-local-variable 'my-prev-whitespace-mode)
  (if whitespace-mode
      (progn
        (setq my-prev-whitespace-mode t)
        (whitespace-mode -1))
    (setq my-prev-whitespace-mode nil)))

(defadvice popup-delete (after my-restore-whitespace)
  "Restore previous whitespace mode when deleting autocomplete box"
  (if my-prev-whitespace-mode
      (whitespace-mode 1)))

(ad-activate 'popup-draw)
(ad-activate 'popup-delete)

;; Comment and Uncomment
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )
(global-set-key (kbd "C-\\") 'comment-or-uncomment-line-or-region)


;; browse-kill-ring+
(require 'browse-kill-ring+)

;; create a new shell
(defun create-shell ()
  "creates a shell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;; Load my personal function lib
(load "~/.emacs.d/vendor/my-functions.el")
(load "~/.emacs.d/vendor/tag_helper.el")

;; clang
;; auto syntax check using Clang for C/C++
(require 'flymake)
(require 'flymake-clang-c)
(add-hook 'c-mode-hook 'flymake-clang-c-load)
(require 'flymake-clang-c++)
(add-hook 'c++-mode-hook 'flymake-clang-c++-load)
;; flymake
;(require 'flymake-settings)
(setq flymake-allowed-file-name-masks
      (cons '("\\.[ch]pp$" ede-compdb-flymake-init)
            flymake-allowed-file-name-masks))
;; flycheck
(require 'flycheck)

;; c/c++
(require 'google-c-style)

;; switching between header and source files
(require 'buftoggle)

;;recentfile list
(recentf-mode 1)
(setq recentf-max-menu-items 15)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(defun c-base-mode-headline ()
  (interactive)
  ;; Set to 500 so it is displayed even if all methods are not narrowed down.
  (let ((anything-candidate-number-limit 500))
    (cond
     ((eq major-mode 'objc-mode)
      (anything-other-buffer '(anything-c-source-objc-headline)
                             "*ObjC Headline*"))
     ((eq major-mode 'c++-mode)
      (anything-other-buffer '(anything-c-source-cpp-headline)
                             "*Cpp Headline*"))
     ((eq major-mode 'c-mode)
      (anything-other-buffer '(anything-c-source-c-headline)
                             "*C Headline*"))
     )))

;; CC-mode setting
(add-to-list 'load-path "~/.emacs.d/vendor/cc-mode-5.32.5")
(require 'cc-mode)      ;cc-mode
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; (setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode-map  [(tab)] 'company-complete)
;(define-key c++-mode-map  [(tab)] 'company-complete)


(defun my-c-mode-common-hook()
  ;; (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  (global-set-key [(control tab)] 'semantic-ia-complete-symbol-menu)
  ;;(define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  (define-key c-mode-base-map (kbd "C-c C-f") 'c-base-mode-headline)
  (define-key c-mode-base-map (kbd "M--") 'senator-fold-tag-toggle)
  (c-set-style "stroustrup")
  )
(add-hook 'c-mode-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)

;; matchit
(require 'matchit)

;; 把pdf,ps,dvi文件转换为png格式, 在Emacs里面浏览
(require 'doc-view)
(setq doc-view-conversion-refresh-interval 3)


;; dash at point
(add-to-list 'load-path "/path/to/dash-at-point")
(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; opam
(setq explicit-bash-args '("--login" "-i"))

(require 'dirtree)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; python
                                        ; Tab = 4 空格 宽度
(setq default-tab-width 4)
                                        ; Tab = 4 空格
(setq-default indent-tabs-mode nil)
