;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; 共通初期設定ファイル
;;-----------------------------------------------------------------------------

;; OS識別用
;(defun windowsp () (string-match "mingw" system-configuration))
(defun windowsp () (string-match "-nt" system-configuration))
(defun linuxp () (string-match "linux" system-configuration))

;; Version識別用
(defun meadowp () (featurep 'meadow))
(defun meadow3p () (and (meadowp)
			(string-match "Meadow-3" (Meadow-version))))
(defun meadow2p () (and (meadowp)
			(string-match "Meadow-2" (Meadow-version))))

;; Migemo使用判別
(defun migemop () (and (>= emacs-major-version 23)
					   (executable-find "cmigemo")))

;;; Mule-UCS
;; ftp://ftp.m17n.org/pub/mule/Mule-UCS/
;; (set-language-environment)
(when (meadow2p)
	  (require 'un-define)
	  (require 'jisx0213))

;;; load-path
(setq load-path
      (append
	   (list
		;;(expand-file-name emacs-dir) ※Emacs 24.5では警告が出る
		(expand-file-name (concat emacs-dir "miya-elisp"))
		(expand-file-name (concat emacs-dir "elisp"))
		)								; list end
       load-path))

;;FTP
;(setq ange-ftp-ftp-program-name "/usr/bin/ftp")

;(require 'browse-url)
(require 'cl)
;(require 'grep-edit)
;(when (require 'undohist nil t)
;  (undohist-initialize))


(when (>= emacs-major-version 24)
  (load-library "miya-el-get.el"))
(load-library "miya-screen.el")
(load-library "miya-font.el")
(load-library "miya-util.el")
(load-library "miya-key.el")
(load-library "miya-func.el")
(load-library "miya-mode.el")
(load-library "miya-dired.el")
(if (require 'helm nil t)
	(load-library "miya-helm.el")
  (load-library "miya-anything.el"))

;; w3m
;; (when (require 'w3m nil t)
;;   (load-library "miya-w3m.el"))

;; mew
;; (when (require 'mew nil t)
;;   (load-library "miya-mew.el"))

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

