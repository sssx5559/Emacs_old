;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; 雑多な設定
;;-----------------------------------------------------------------------------

;; ;;; shell の設定

;; ;;; Cygwin の bash を使う場合
(setq explicit-shell-file-name "bash")
(setq shell-file-name "sh")
(setq shell-command-switch "-c")

;; ;;; Virtually UN*X!にある tcsh.exe を使う場合
;; (setq explicit-shell-file-name "tcsh.exe")
;; (setq shell-file-name "tcsh.exe")
;; (setq shell-command-switch "-c")

;; ;;; WindowsNT に付属の CMD.EXE を使う場合。
; (setq explicit-shell-file-name "CMD.EXE")
; (setq shell-file-name "CMD.EXE")
; (setq shell-command-switch "\\/c")

;;; shell coding-system の設定
(when (meadowp)
  (add-hook 'shell-mode-hook
			(lambda ()
			  (set-buffer-process-coding-system 'undecided-dos 'sjis-unix))))


;; ;;; coding-system の設定
;; (modify-coding-system-alist 'process ".*sh\\.exe" 'undecided-dos)


;;; argument-editing の設定
(when (meadowp)
  (if (string= window-system "w32")
	  (require 'mw32script)
	(mw32script-init)))

;; バッファの最後の行で next-line しても新しい行を作らない
(setq next-line-add-newlines nil)

;; 自動改行OFF
(setq require-final-newline nil)

;; バッファの最初の行で previous-line しても、
;; "beginning-of-buffer" と注意されないようにする。
(defun previous-line (arg)
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (line-move (- arg))
        ((beginning-of-buffer end-of-buffer)))
    (line-move (- arg)))
  nil)


;; C-k(kill-line) で行末の改行も含めて kill する
(setq kill-whole-line t)


;; 起動するディレクトリ
(cd "~")


;; 警告音を変更する
;; SOUND: 'asterisk, 'exlamation, 'hand, 'question, 'ok
;(set-message-beep 'hand)


;; 警告音のかわりに画面フラッシュ
(setq visible-bell t)


;;; IMEの設定 + IME の制御(ミニバッファの選択時に半角英数字入力に切り替え)
;; (if (and (meadowp) (string= window-system "w32"))
;; 	(progn
;; 	  (mw32-ime-initialize)
;; 	  (setq default-input-method "MW32-IME")
;; 	  (setq-default mw32-ime-mode-line-state-indicator "[--]")
;; 	  (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
;; 	  (add-hook 'mw32-ime-on-hook
;; 				(function (lambda ()
;; 							(set-cursor-height 2)
;; 							(set-cursor-color "brown")))
;; 	  (add-hook 'mw32-ime-off-hook
;; 				(function (lambda ()
;; 							(set-cursor-height 4)
;; 							(set-cursor-color "pink")))))
;; 	  (wrap-function-to-control-ime 'y-or-n-p nil nil)
;; 	  (wrap-function-to-control-ime 'yes-or-no-p nil nil)))


;; リージョンをハイライト表示する
(transient-mark-mode t)
(setq mark-even-if-inactive t)	; リージョンが不活性でもマークを消さない

;; 行の折り返し設定
(setq truncate-partial-width-windows t)

;; 対応する括弧を光らせる
(show-paren-mode t)

;; ぴーぴーいわないようにする
(setq visible-bell t)


;; リージョンをバックスペースで削除
(when transient-mark-mode
  (defadvice backward-delete-char-untabify
    (around delete-region-like-windows activate)
    (if mark-active
        (delete-region (region-beginning) (region-end))
      ad-do-it)))


;; リージョンを"\C-h"で削除
(when transient-mark-mode
  (defadvice backward-delete-char
    (around delete-region-like-windows activate)
    (if mark-active
        (delete-region (region-beginning) (region-end))
      ad-do-it)))


;; バッファローカル変数デフォルト値設定
(defun miya-make-list (interval up-bound)
  (let ((temp (list up-bound)))
	(while (> up-bound interval)
	  (setq up-bound (- up-bound interval))
	  (setq temp (cons up-bound temp)))
;	(copy-sequence temp)))
	temp))

(setq-default tab-width 4				;タブ幅を 4 に設定
			  truncate-lines t			;切り捨て表示
			  tab-stop-list	(miya-make-list 4 120)) ;タブストップ

;;タブではなくスペースを使う
;(setq indent-tabs-mode nil)
;(setq indent-line-function 'indent-relative-maybe)


;; マクロ設定
(fset 'main
   "public static void main(String[] args){")

;; 関数使用許可設定
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-faces)
(put 'narrow-to-region 'disabled nil)

;; 文字コード、改行コード変換
;(set-buffer-coding-system "コード指定")

;; 検索時の大文字、小文字の区別(t だと区別しない)
;(setq case-fold-search t)

;; ブックマーク自動保存
(setq bookmark-save-flag 1)

;; 圧縮ファイルの編集
(auto-compression-mode)

;; ミニバッファ拡大抑止
;(setq resize-mini-windows nil)


;; レジスタの中にファイル名を保存する方法(C-x r jでファイルにジャンプ)
;(set-register ?レジスタ名 '(file . "ファイルパス"))

;; find-file(C-x C-f) hook の使用例
;(add-hook 'find-file-hooks
;          (function (lambda ()
;                  (if (string-match "makoto/writing" buffer-file-name)
;            (setq fill-column 44))
;                  (if (string-match "makoto/writing/jk" buffer-file-name)
;            (setq fill-column 84))
;                  (if (string-match "vertical/" buffer-file-name)
;            (setq fill-column 40))
;                  (if (string-match "Mail/draft/" buffer-file-name)
;            (setq fill-column 60)) ;; not good now
;                  (if (string-match "makoto/diary/" buffer-file-name)
;            (setq default-buffer-file-coding-system 'euc-japan)
;            (setq default-buffer-file-coding-system 'iso-2022-jp ))
;                  )))

;; C言語で"FiXME"という単語を強調表示する例
; (font-lock-add-keywords
;  'c-mode
;  '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))

;;; gnuserv
(when (require 'gnuserv nil t)
  (gnuserv-start)
  (setq gnuserv-frame (selected-frame))) ; 新しいフレームを開かない

;; デフォルトメジャーモード設定
(setq default-major-mode 'lisp-interaction-mode)

;; 補完モードON
(setq-default abbrev-mode t)

;; タブストップリスト作成用
(defun miya-make-tab-stop-list ()
  (let ((index 120)
		(def 4)
		(list))
	(while (>= index def)
	  (setq list (cons index list))
	  (setq index (- index def)))
	list))

;; タブストップリスト作成
(setq-default tab-stop-list (miya-make-tab-stop-list))

;; (setq make-backup-files nil)			;; ファイルバックアップなし
(setq mode-require-final-newline nil)	;; 保存時に改行コードをつけない

;; 日付入力
(autoload 'instamp "instamp" "Insert TimeStamp on the point" t)
(define-key global-map "\C-cs" 'instamp)

;; grep
(setq grep-command "grep -rne ")


;; ffap
;(ffap-bindings)	;; バッファ上のカーソル位置からパスを見つける

;; filecache
;(require 'filecache)
;(file-cache-add-directory-list
; (list "~" "e:/Meadow"))
;(define-key minibuffer-local-completion-map
;  "\C-c\C-i" 'file-cache-minibuffer-complete)


;; キルリング一覧表示モード
;(autoload 'kill-summary "kill-summary" nil t)
;(define-key global-map "\ey" 'kill-summary)

;; <C-x C-c>終了時にプロセスを自動終了
;(defadvice save-buffers-kill-terminal (before my-save-buffers-kill-terminal activate)
;  (when (process-list)
;    (dolist (p (process-list))
;      (set-process-query-on-exit-flag p nil))))

;;=========================================================
;; ediff
;;=========================================================
;; Ediff Control Panelを同じフレーム内に表示する
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 差分を横に分割して表示する
(setq ediff-split-window-function 'split-window-horizontally)
;; 余計なバッファを(確認して)削除する
(setq ediff-keep-variants nil)

;;=========================================================
;; mouse
;;=========================================================
;; マウスホイールでスクロール
(global-set-key (kbd "<wheel-up>")
				'(lambda () (interactive) (scroll-down 3)))
(global-set-key (kbd "<wheel-down>")
				'(lambda () (interactive) (scroll-up 3)))

;; スクロールステップ 3に設定
;(setq scroll-step 3)

;;=========================================================
;; find-file
;;=========================================================
;; 大文字、小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;=========================================================
;; 圧縮ファイルを編集
;;=========================================================
(auto-compression-mode t)

;;=========================================================
;; キーボードマクロを保存
;;=========================================================
(defvar kmacro-save-file "~/kmacro-save.el")

(defun kmacro-save (symbol)
  (interactive "SName for last kbd macro: ")
  (name-last-kbd-macro symbol)		; 名前を付ける
  (with-current-buffer (find-file-noselect kmacro-save-file)
	(goto-char (point-max))
	(insert-kbd-macro symbol)
	(basic-save-buffer)))

;;=========================================================
;; eshell
;;=========================================================
;(require 'esh-myparser)

;;=========================================================
;; hippie-expand
;;=========================================================
;; 略語展開機能選択
(setq hippie-expand-try-functions-list
	  '(
		try-complete-file-name-partially
		try-complete-file-name
;;		try-expand-all-abbrevs
		try-expand-list
		try-expand-line
		try-expand-dabbrev
		try-expand-dabbrev-all-buffers
		try-expand-dabbrev-from-kill
		try-complete-lisp-symbol-partially
		try-complete-lisp-symbol
		))
