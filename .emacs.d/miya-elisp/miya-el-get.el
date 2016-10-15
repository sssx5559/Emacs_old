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


;;-----------------------------------------------------------------------------
;; インストールパッケージ
;;-----------------------------------------------------------------------------

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

;;=========================================================
;; ac-complete
;;=========================================================
;; ;; ※何故かel-get-bundleの中の初期化が有効にならない・・・(Emacs 24.5.1ではOK)
;; (el-get-bundle auto-complete
;;   (require 'auto-complete)
;;   (require 'auto-complete-config)
;;   (global-auto-complete-mode t)

;;   ;; 補完メニュー時のキーマップ
;;   (define-key ac-completing-map (kbd "C-n") 'ac-next)
;;   (define-key ac-completing-map (kbd "C-p") 'ac-previous)

;;   ;; トリガーキーの設定
;;   (ac-set-trigger-key (kbd "TAB"))
;;   ;(setq ac-auto-start 3)	 ; 3文字以上で自動補完
;;   (setq ac-auto-start nil)	 ;自動補完しない
;;   ;(define-key ac-mode-map (kbd "C-c /") 'auto-complete)
;;   (global-set-key (kbd "C-c /") 'auto-complete)

;;   ;(setq ac-auto-show-menu nil)	;補完メニューを自動表示しない
;;   ;(setq ac-auto-show-menu 0.8) ;補完メニュー表示時間(0.8s)

;;   ;; TABで補完完了、リターンは改行のみの設定
;;   (define-key ac-completing-map "\t" 'ac-complete)
;;   ;(define-key ac-completing-map "\r" nil

;;   ;(setq ac-dwim t)  ; 空気読んでほしい(デフォルトON)

;;   ;; 情報源として
;;   ;; * ac-source-filename
;;   ;; * ac-source-words-in-same-mode-buffers
;;   ;; を利用
;;   (setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
;;   ;; また、Emacs Lispモードではac-source-symbolsを追加で利用
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))

;;   ;(setq ac-ignore-case t)		; 大文字・小文字を区別しない
;;   (setq ac-ignore-case 'smart)	; 補完対象に大文字が含まれる場合のみ区別する
;;   ;(setq ac-ignore-case nil)	; 大文字・小文字を区別する  (require 'auto-complete-config)
;;   )

;;=========================================================
;; company
;;=========================================================
(el-get-bundle company-mode
  (require 'company)

  (global-company-mode)					 ; 全バッファで有効にする
  (setq company-idle-delay 0)			 ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

  ;; 自動補完を offにしたい場合は, company-idle-delayを nilに設定する
  ;; auto-completeでいうところの ac-auto-start にあたる.
  (custom-set-variables
   '(company-idle-delay nil))

  ;;;;;;;;;;;;;;;;;;;;
  ;; 画面設定
  ;;;;;;;;;;;;;;;;;;;;
  (set-face-attribute 'company-tooltip nil
					  :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
					  :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
					  :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
					  :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
					  :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
					  :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
					  :background "gray40")

  ;;;;;;;;;;;;;;;;;;;;
  ;; キー設定
  ;;;;;;;;;;;;;;;;;;;;
  (global-set-key (kbd "C-i") 'company-complete)

  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") 'delete-backward-char)
  (define-key company-active-map (kbd "C-o") 'company-show-doc-buffer)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-h") 'delete-backward-char)
  (define-key company-search-map (kbd "C-o") 'company-show-doc-buffer)

  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)

  ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

  ;;=========================================================
  ;; company-jedi(Python入力補完)
  ;;=========================================================
  (el-get-bundle jedi-core)
  (el-get-bundle company-jedi
  	(require 'company-jedi)
  	(add-hook 'python-mode-hook 'jedi:setup)
  	(add-to-list 'company-backends 'company-jedi) ; backendに追加

  	(setq jedi:complete-on-dot t)
  	(setq jedi:use-shortcuts t)
  ))

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
  (defun my-quickrun ()
	(interactive)
	(if mark-active
		(quickrun :start (region-beginning) :end (region-end))
	  (quickrun)))
  )

;; popwin
;(el-get-bundle popwin)

;; popup-select-window ※Ubuntu 16.04だとパッケージを取得できない。
(el-get-bundle popup)
(el-get-bundle popup-select-window
  (require 'popup-select-window)
  (global-set-key "\C-xo" 'popup-select-window)) ; other-windowを上書き
;; (when (require 'popup-select-window nil t)
;;   (global-set-key "\C-xo" 'popup-select-window)) ; other-windowを上書き

;; Erlang mode
(when (executable-find "erl")
  (el-get-bundle erlang-mode))

;; Elixir mode
(when (executable-find "elixir")
  (el-get-bundle pkg-info)		; elixirで使用
  (el-get-bundle elixir))

;; Everything
;; (when (windowsp)
;;   (el-get-bundle everything
;; 	(require 'everything)))
