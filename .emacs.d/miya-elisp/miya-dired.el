;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; dired設定
;;-----------------------------------------------------------------------------

;;=========================================================
;; 関連付けられたファイルを開く
;;=========================================================
;; (defun open-file-dwim (filename)
;;   "Open file dwim"
;;   (let* ((winp (string-equal window-system "w32"))
;; 		 ;; xlsmファイル対策
;;          (opener
;; 		  (cond ((file-directory-p filename) (if winp '("explorer.exe") '("gnome-open")))
;; ;				((string= "xlsm" (file-name-extension filename)) '("C:/Program Files/Microsoft Office/Office14/EXCEL.exe"))
;; 				((string= "xlsm" (file-name-extension filename)) '("explorer.exe"))
;; 				 (t (if winp '("fiber.exe") '("gnome-open")))))

;;          (fn (replace-regexp-in-string "/$" "" filename))
;;          (args (append opener (list (if winp
;;                                         (replace-regexp-in-string "/" (rx "\\") fn)
;;                                       fn))))
;;          (process-connection-type nil))
;;     (apply 'start-process "open-file-dwim" nil args)))

(defun open-file-dwim-win (filename)
  "Open file dwim for Windows"
  (w32-shell-execute "open" filename))

(defun open-file-dwim-mac (filename)
  "Open file dwim for Mac"
  (let ((process-connection-type nil))	;; これを設定しないと上手く表示されない
    (start-process "open-mac" nil "open" filename)))

(defun open-file-dwim-linux (filename)
  "Open file dwim for Linux"
  (let ((process-connection-type nil))	;; これを設定しないと上手く表示されない
    (start-process "xdg-open" nil "xdg-open" filename)))

;; カーソル下のファイルやディレクトリを関連付けられたプログラムで開く
(defun dired-my-open-dwim ()
  "Open file under the cursor"
  (interactive)
  (cond
   ((windowsp)
	(open-file-dwim-win (dired-get-filename)))
   ((macp)
	(open-file-dwim-mac (dired-get-filename)))
   ((linuxp)
	(open-file-dwim-linux (dired-get-filename)))
   (t
	;; Nothing
	)))

;;=========================================================
;; スペースでマークする (FD like)
;;=========================================================
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    (dired-previous-line 1)))

;;=========================================================
;; dired を使って、ファイルの coding system (漢字) を変換する
;;=========================================================
(require 'dired-aux)

(defvar dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")

(defvar dired-file-coding-system 'no-conversion)

(defun dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-do-convert-coding-system (coding-system &optional arg)
  "Convert file (s) in specified coding system."
  (interactive
   (list (let ((default (or dired-default-file-coding-system
                            buffer-file-coding-system)))
           (read-coding-system
            (format "Coding system for converting file (s) (default, %s): "
                    default)
            default))
         current-prefix-arg))
  (check-coding-system coding-system)
  (setq dired-file-coding-system coding-system)
  (dired-map-over-marks-check
   (function dired-convert-coding-system) arg 'convert-coding-system t))

;;=========================================================
;; フォルダを開く時, 新しいバッファを作成しない
;;=========================================================
(defun dired-my-advertised-find-file ()
  (interactive)
  (let ((kill-target (current-buffer))
		(check-file (dired-get-filename)))
	(funcall 'dired-advertised-find-file)
	(if (file-directory-p check-file)
		(kill-buffer kill-target))))

(defun dired-my-up-directory (&optional other-window)
  "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
  (interactive "P")
  (let* ((dir (dired-current-directory))
		 (up (file-name-directory (directory-file-name dir))))
	(or (dired-goto-file (directory-file-name dir))
		;; Only try dired-goto-subdir if buffer has more than one dir.
		(and (cdr dired-subdir-alist)
			 (dired-goto-subdir up))
		(progn
		  (if other-window
			  (dired-other-window up)
			(progn
			  (kill-buffer (current-buffer))
			  (dired up))
			(dired-goto-file dir))))))

;; ファイルを開くときはバッファを削除しない
(defun dired-my-find-alternate-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;;=========================================================
;; 今日変更したファイルに色をつける
;; ※NTEmacsだと動かない・・・
;;=========================================================
;; (defface face-file-edited-today
;;   '((((class color)
;;       (background dark))
;;      (:foreground "GreenYellow"))
;;     (((class color)
;;       (background light))
;;      (:foreground "magenta"))
;;     (t
;;      ())) nil)
;; (defvar face-file-edited-today
;;   'face-file-edited-today)
;; (defun my-dired-today-search (arg)
;;   "Fontlock search function for dired."
;;   (search-forward-regexp
;;    (concat "\\(" (format-time-string
;;                   "%b %e" (current-time))
;;            "\\|"(format-time-string
;;                  "%m-%d" (current-time))
;;            "\\)"
;;            " [0-9]....") arg t))
;; (font-lock-add-keywords
;;  major-mode
;;  (list
;;   '(my-dired-today-search . face-file-edited-today)
;;   ))

;;=========================================================
;; ディレクトリを上に表示
;;=========================================================
;(require 'ls-lisp)
;(setq ls-lisp-dirs-first t)

;;=========================================================
;; サイズ，拡張子で並び換え
;;=========================================================
;(add-hook 'dired-load-hook
;          (lambda () (require 'sorter)))
(load "sorter")

;;=========================================================
;; wdired
;;=========================================================
(require 'wdired nil t)

;;=========================================================
;; キーバインド
;;=========================================================
(add-hook 'dired-mode-hook
          (lambda ()
			(define-key dired-mode-map (kbd "e") 'dired-my-open-dwim) 				; 関連付けられたPrgで開く
 			(define-key dired-mode-map (kbd " ") 'dired-toggle-mark)
            (define-key dired-mode-map (kbd "T") 'dired-do-convert-coding-system)
			(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

			;; 下の階層に移動 or ファイルを開く
			(define-key dired-mode-map (kbd "RET") 'dired-my-find-alternate-file)	; 新規バッファを作らない
			(define-key dired-mode-map (kbd "f") 'dired-my-find-alternate-file)		; 新規バッファを作らない
			(define-key dired-mode-map (kbd "a") 'dired-find-file) 					; 新規バッファを作る

			;; 上の階層に移動
			(define-key dired-mode-map (kbd "c") 'dired-my-up-directory) 			; 新規バッファを作らない
			(define-key dired-mode-map (kbd "b") 'dired-my-up-directory) 			; 新規バッファを作らない
			(define-key dired-mode-map (kbd "^") 'dired-up-directory) 				; 新規バッファを作る

			(when (require 'dired-hacks-utils nil t)
			  (define-key dired-mode-map (kbd "n") 'dired-hacks-next-file) 			; 既存動作を上書き
			  (define-key dired-mode-map (kbd "p") 'dired-hacks-previous-file) 		; 既存動作を上書き

			  (define-key dired-mode-map (kbd "o") 'dired-subtree-insert)
			  (define-key dired-mode-map (kbd "O") 'dired-subtree-remove)
			  )
			))

;;=========================================================
;; カスタム変数定義
;;=========================================================
(custom-set-variables
 ;; diredを2つのウィンドウで開いている時に、
 ;; デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
 '(dired-dwim-target t)

 '(dired-default-file-coding-system 'utf-8)	; デフォルトの文字コード
 '(dired-recursive-copies 'always) 			; 再帰コピー
 '(dired-recursive-deletes 'always)			; 再帰削除
 '(dired-listing-switches "-alh")			; ファイルサイズを見やすくする
 )
