;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; 共通初期設定ファイル
;;-----------------------------------------------------------------------------

;; OS識別用
(defun windowsp () (or (string-match "mingw" system-configuration)
					   (string-match "-nt" system-configuration)))
(defun linuxp () (string-match "linux" system-configuration))
(defun macp () (string-match "apple" system-configuration))

;; Meadow識別用
(defun meadowp () (and (featurep 'meadow)
					   (string-match "Meadow-3" (Meadow-version))))

;; Migemo使用判別
(defun migemop () (and (>= emacs-major-version 23)
					   (executable-find "cmigemo")))

;; Warningを抑止
;;
;; [el-get]
;; Warning (el-get): Your Emacs doesn't support HTTPS (TLS),
;;  see https://github.com/dimitri/el-get/wiki/Installation-on-Windows.
(when (windowsp)
  (custom-set-variables
;;   '(display-warning-minimum-level :error)
   '(warning-minimum-level :error)
   ))

;;; load-path
(setq load-path
      (append
	   (list
		;;(expand-file-name emacs-dir) ※Emacs 24.5では警告が出る
		(expand-file-name (concat emacs-dir "miya-elisp"))
		(expand-file-name (concat emacs-dir "elisp"))
		)								; list end
       load-path))

;; custom-theme-load-path
(unless (meadowp)
  (add-to-list 'custom-theme-load-path
			   (expand-file-name (concat emacs-dir "themes")))
  )

;;======================================================
;; 設定ファイル読み込み
;;======================================================
(if (meadowp)
	(load-library "miya-meadow.el")
  (load-library "miya-el-get.el"))

(load-library "miya-screen.el")
(load-library "miya-font.el")
(load-library "miya-util.el")
(load-library "miya-key.el")
(load-library "miya-func.el")
(load-library "miya-mode.el")
(load-library "miya-dired.el")
(if (featurep 'helm)
	(load-library "miya-helm.el")
  (load-library "miya-anything.el"))

(load-library "work.el")

;;======================================================
;; Emacs操作メモ
;;======================================================
; (replace-string)での改行入力
; ・C-q, C-jと入力するとラインが変わるので、そこで改行を入力する。
;
; 複数ファイル名変更
; ・diredモード(C-x d)で M-x dired-mark-files-regexpで'*'マークをつける
;   M-x dired-do-rename-regexpで置換部分を指定する
;
; TAGファイル置換
; ・"etags ./**/*.[cChH]"でTAGファイル作成
;   tags-query-replaceで置換


; C-c > runs the command python-shift-right
; C-c < runs the command python-shift-left

;; migemo isearch on/off設定
; 有効 (setq migemo-isearch-enable-p t)
; 無効 (setq migemo-isearch-enable-p nil)

;; wgrep(ag版)使い方
; 1. 検索結果バッファ上でr → 編集できるように
; 2. C-x C-s (もしくは C-c C-e or C-c C-c) で検索元のファイルを更新
; 3. M-x wgrep-save-all-buffers で更新を保存


;; ivy-modeで、候補があることによりファイルやフォルダを作成できないとき
;; C-M-j (https://qiita.com/xargs/items/d54ee21a55c10ece2eeb)
