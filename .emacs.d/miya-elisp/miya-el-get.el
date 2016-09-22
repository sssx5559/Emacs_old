;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; El-Get設定ファイル
;;-----------------------------------------------------------------------------
;; load-path で ~/.emacs.d とか書かなくてよくなる
;;(when load-file-name
;;  (setq user-emacs-directory (file-name-directory load-file-name)))
(setq user-emacs-directory emacs-dir)

;; パッケージのインストール先をEmacsバージョンによって変える
(let ((versioned-dir (locate-user-emacs-file emacs-version)))
  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
        package-user-dir (expand-file-name "elpa" versioned-dir)))

;; el-getでダウンロードしたパッケージ置き場
;(setq el-get-dir (locate-user-emacs-file "el-get/package"))

;;(when my-el-get-dir
  ;;  (add-to-list 'load-path (locate-user-emacs-file my-el-get-dir))
  (add-to-list 'load-path (concat el-get-dir "/el-get"))
  (unless (require 'el-get nil 'noerror)
	;; El-Getインストール未
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
	  (goto-char (point-max))
	  (eval-print-last-sexp)))

;;===============================================
;; インストールパッケージ
;;===============================================
;; exec-path-from-shell
(when (macp)
  (el-get-bundle exec-path-from-shell
	(exec-path-from-shell-initialize)))

;; Migemo
(when (migemop)
  (el-get-bundle migemo))

;; Helm
(when (>= emacs-minor-version 3)
  ;; Emacs Version >= 24.3
  (el-get-bundle helm)
  (el-get-bundle helm-ag)
  (el-get-bundle helm-descbinds)
  (el-get-bundle helm-project)
  (el-get-bundle helm-gtags)
  (el-get-bundle helm-swoop)
  (el-get-bundle helm-ls-git))

;; The Silver Searcher
(when (executable-find "ag")
  (el-get-bundle cl-lib)				; これを入れないとエラーになる
  (el-get-bundle ag)
  (el-get-bundle wgrep))

(el-get-bundle wgrep
  (require 'ag)
  (require 'wgrep-ag)
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)

  ;; agの検索結果バッファで"r"で編集モード
  ;; C-x C-sで保存して終了、C-x C-kで保存せずに終了
  (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode))

;; ace-isearch
;; (el-get-bundle ace-isearch
;;   (require 'ace-isearch)
;;   (global-ace-isearch-mode t)
;;   )


;; ※何故かel-get-bundleの中の初期化が有効にならない・・・(Emacs 24.5.1ではOK)
(el-get-bundle auto-complete
  (require 'auto-complete)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)

  ;; 補完メニュー時のキーマップ
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)

  ;; トリガーキーの設定
  (ac-set-trigger-key "TAB")
  ;(setq ac-auto-start 3)	 ; 3文字以上で自動補完
  (setq ac-auto-start nil)	 ;自動補完しない
  ;(define-key ac-mode-map (kbd "C-c /") 'auto-complete)
  (global-set-key (kbd "C-c /") 'auto-complete)

  ;(setq ac-auto-show-menu nil)	;補完メニューを自動表示しない
  ;(setq ac-auto-show-menu 0.8) ;補完メニュー表示時間(0.8s)

  ;; TABで補完完了、リターンは改行のみの設定
  ;(define-key ac-completing-map "\t" 'ac-complete)
  ;(define-key ac-completing-map "\r" nil

  ;(setq ac-dwim t)  ; 空気読んでほしい(デフォルトON)

  ;; 情報源として
  ;; * ac-source-filename
  ;; * ac-source-words-in-same-mode-buffers
  ;; を利用
  (setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
  ;; また、Emacs Lispモードではac-source-symbolsを追加で利用
  (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))

  ;(setq ac-ignore-case t)		; 大文字・小文字を区別しない
  (setq ac-ignore-case 'smart)	; 補完対象に大文字が含まれる場合のみ区別する
  ;(setq ac-ignore-case nil)	; 大文字・小文字を区別する  (require 'auto-complete-config)
  )

;; multiple-cursors
(el-get-bundle multiple-cursors
  (require 'multiple-cursors)

  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

  ;; (defun mc/mark-all-dwim-or-expand-region (arg)
  ;; 	(interactive "p")
  ;; 	(cl-case arg
  ;; 	  (16 (mc/mark-all-dwim t))
  ;; 	  (4 (mc/mark-all-dwim nil))
  ;; 	  (1 (call-interactively 'er/expand-region))))

  ;; ;; C-M-SPCでer/expand-region
  ;; ;; C-u C-M-SPCでmc/mark-all-in-region
  ;; ;; C-u C-u C-M-SPCでmc/edit-lines
  ;; (global-set-key (kbd "C-M-SPC") 'mc/mark-all-dwim-or-expand-region)


  ;; (defun mc/edit-lines-or-string-rectangle (s e)
  ;; 	"C-x r tで同じ桁の場合にmc/edit-lines (C-u M-x mc/mark-all-dwim)"
  ;; 	(interactive "r")
  ;; 	(if (eq (save-excursion (goto-char s) (current-column))
  ;; 			(save-excursion (goto-char e) (current-column)))
  ;; 		(call-interactively 'mc/edit-lines)
  ;; 	  (call-interactively 'string-rectangle)))

  ;; (global-set-key (kbd "C-x r t") 'mc/edit-lines-or-string-rectangle)

  ;; (defun mc/mark-all-dwim-or-mark-sexp (arg)
  ;; 	"C-u C-M-SPCでmc/mark-all-dwim, C-u C-u C-M-SPCでC-u M-x mc/mark-all-dwim"
  ;; 	(interactive "p")
  ;; 	(cl-case arg
  ;; 	  (16 (mc/mark-all-dwim t))
  ;; 	  (4 (mc/mark-all-dwim nil))
  ;; 	  (1 (mark-sexp 1))))

  ;; (global-set-key (kbd "C-M-SPC") 'mc/mark-all-dwim-or-mark-sexp)
  )

;; quickrun
(el-get-bundle quickrun

  ;; region選択:quickrun-region, 非選択:quickrun
  (defun my-quickrun (start end)
	(interactive "r")
	(if mark-active
		(quickrun :start start :end end)
	  (quickrun)))
  )

;; popwin
(el-get-bundle popwin)

;; Erlang mode
(when (executable-find "erl")
  (el-get-bundle erlang-mode))

;; Elixir mode
(when (executable-find "elixir")
  (el-get-bundle pkg-info)		; elixirで使用
  (el-get-bundle elixir))
