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

;; 暫定
;;(setq el-get-dir (expand-file-name emacs-version (locate-user-emacs-file "el-get")))
;;
;;(add-to-list 'load-path (concat el-get-dir "/el-get"))
;;
;;(unless (require 'el-get nil 'noerror)
;;  ;; El-Getインストール未
;;  (load-library "el-get-install.el"))


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

;;=========================================================
;; Helm
;;========================================================
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-project)
(el-get-bundle helm-gtags)
(el-get-bundle helm-swoop)
(el-get-bundle helm-descbinds)

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

;;=========================================================
;; ace-isearch
;;=========================================================
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

  (custom-set-variables
   '(company-idle-delay nil)			; 自動補完off デフォルトは0.5
   '(company-minimum-prefix-length 2)	; デフォルトは4
   '(company-selection-wrap-around t)	; 候補の一番下でさらに下に行こうとすると一番上に戻る
   ;;'(company-transformers nil)			; 補完候補の順番を指定(nilデフォルト)
   )

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
;;  (global-set-key (kbd "C-i") 'company-complete)
;;  (global-set-key (kbd "<tab>") 'company-complete)
  (global-set-key (kbd "M-i") 'company-complete)

  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;;; C-hのドキュメント表示を変更
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-o") 'company-show-doc-buffer)

  ;;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
;;  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "M-i") 'company-complete-common-or-cycle)

  ;; 絞り込み検索
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; 候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)

  ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-h") nil)
  (define-key company-search-map (kbd "C-o") 'company-show-doc-buffer)
;;  (define-key company-search-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-search-map (kbd "M-i") 'company-complete-common-or-cycle)

  ;; quick-help
  ;; (el-get-bundle pos-tip)
  ;; (el-get-bundle company-quickhelp
  ;; 	(company-quickhelp-mode +1)		;; 使うと重たくなる
  ;; 	)

  ;;=========================================================
  ;; company-jedi(Python入力補完)
  ;;=========================================================
  (el-get-bundle jedi-core)
  (el-get-bundle company-jedi
  	(require 'company-jedi)
  	(add-hook 'python-mode-hook 'jedi:setup)
  	(add-to-list 'company-backends 'company-jedi) ; backendに追加

	(custom-set-variables
	 '(jedi:complete-on-dot t)
	 '(jedi:use-shortcuts t)
	 )
  ))

;;=========================================================
;; yasnippet
;;=========================================================
(el-get-bundle yasnippet
  (require 'yasnippet)

  (custom-set-variables
   ;; '(yas-snippet-dirs
   ;; 	 (list (const user-emacs-directory "snippets")				  	;; 自作用(省略可能)
   ;; 		   (const user-emacs-directory "elisp/yasnippet/snippets")	;; 最初から入っていたスニペット(省略可能)
   ;; 		   ))
   )

  ;; 既存スニペットを挿入する
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
  ;; 新規スニペットを作成するバッファを用意する
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  ;; 既存スニペットを閲覧・編集する
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

  ;; (when (require 'helm nil t)
  ;; 	(el-get-bundle helm-c-yasnippet
  ;; 	  (custom-set-variables
  ;; 	   '(helm-yas-space-match-any-greedy t)
  ;; 	   )
  ;; 	  (global-set-key (kbd "C-c y") 'helm-yas-complete)))

  ;; (eval-after-load "yasnippet"
  ;; '(progn
  ;;    ;; companyと競合するのでyasnippetのフィールド移動は "C-i" のみにする
  ;;    (define-key yas-keymap (kbd "<tab>") nil)
  ;;    (yas-global-mode 1))))

  (yas-global-mode 1)
  )

;;=========================================================
;; smartrep(連続操作を楽にする)
;;=========================================================
(el-get-bundle smartrep
  (require 'smartrep)
  ;; 非アクティブウインドウをスクロール
  (global-unset-key (kbd "C--"))
  (smartrep-define-key global-map "C--"
	'(("n" . (scroll-other-window 1))
	  ("p" . (scroll-other-window -1))
	  ("N" . 'scroll-other-window)
	  ("P" . (scroll-other-window '-))
	  ("a" . (beginning-of-buffer-other-window 0))
	  ("e" . (end-of-buffer-other-window 0))
	  ("-" . 'shrink-window)
	  ("+" . (shrink-window -1))
	  ("{" . 'shrink-window-horizontally)
	  ("}" . (shrink-window-horizontally -1))
	  ))
  )

;;=========================================================
;; multiple-cursors
;;=========================================================
(el-get-bundle multiple-cursors
  (require 'multiple-cursors)
  (require 'smartrep)

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

  (declare-function smartrep-define-key "smartrep")

  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-M-m") 'mc/mark-all-in-region)

  (global-unset-key "\C-t")

  (smartrep-define-key global-map "C-t"
	'(("C-t"  . 'mc/mark-next-like-this)
	  ("n"    . 'mc/mark-next-like-this)
	  ("p"    . 'mc/mark-previous-like-this)
	  ("m"    . 'mc/mark-more-like-this-extended)
	  ("N"    . 'mc/unmark-next-like-this)
	  ("P"    . 'mc/unmark-previous-like-this)
	  ("s"    . 'mc/skip-to-next-like-this)
	  ("S"    . 'mc/skip-to-previous-like-this)
	  ("*"    . 'mc/mark-all-like-this)
	  ("d"    . 'mc/mark-all-like-this-dwim)
	  ("i"    . 'mc/insert-numbers)
	  ("o"    . 'mc/sort-regions)
	  ("O"    . 'mc/reverse-regions)
	  ))
  )

;;=========================================================
;; iedit
;;=========================================================
(el-get-bundle victorhge/iedit
  (require 'iedit)
  (setq iedit-toggle-key-default nil) ;; "C-c ;"を設定しても使えない
  (global-set-key (kbd "C-c ;") 'iedit-mode)

  (define-key iedit-mode-keymap (kbd "C-m") 'iedit-toggle-selection)
  (define-key iedit-mode-keymap (kbd "M-p") 'iedit-expand-up-a-line)
  (define-key iedit-mode-keymap (kbd "M-n") 'iedit-expand-down-a-line)
  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line)
  (define-key iedit-mode-keymap (kbd "C-g") 'iedit-mode) ;; Exit iedit-mode
  (define-key iedit-mode-keymap (kbd "C-h") 'delete-backward-char)
  )

;;=========================================================
;; quickrun
;;=========================================================
(el-get-bundle quickrun
  ;; region選択:quickrun-region, 非選択:quickrun
  (defun my-quickrun ()
	(interactive)
	(if mark-active
		(quickrun :start (region-beginning) :end (region-end))
	  (quickrun)))

  (when (boundp 'my-python)
	;; Override existing command
	(quickrun-add-command "python"
						  (list (cons :command my-python))
						  :override t))

  ;; (custom-set-variables
  ;;  '(quickrun-option-outputter 'message)	;; 実行結果をエコーエリアに出力
  ;;  )
  )

;;=========================================================
;; popup-select-window ※Ubuntu 16.04だとパッケージを取得できない。
;;=========================================================
(el-get-bundle popup)

;; (el-get-bundle popup)の中に入れると有効にならない。
(when (require 'popup-select-window nil t)
  (global-set-key (kbd "C-x o") 'popup-select-window))	; other-windowを上書き

;;=========================================================
;; Erlang mode
;;=========================================================
;; (when (executable-find "erl")
;;   (el-get-bundle erlang-mode))

;;=========================================================
;; Elixir mode
;;=========================================================
(when (executable-find "elixir")
  (el-get-bundle elixir :depends (pkg-info)))

;;=========================================================
;; undo-tree
;;=========================================================
(el-get-bundle undo-tree
  (global-undo-tree-mode)
  ;; redoキー設定
  (define-key global-map (kbd "C-M-/") 'undo-tree-redo)
  )

;;=========================================================
;; expand-region
;;=========================================================
(el-get-bundle expand-region
  (global-set-key (kbd "C-@") 'er/expand-region)
  (global-set-key (kbd "C-M-@") 'er/contract-region)
  )

;;=========================================================
;; smartparents
;;=========================================================
;; (el-get-bundle smartparens
;;   (smartparens-global-mode t)
;; ;; (show-smartparens-global-mode)	; 重くなるかも
;;   )

;;=========================================================
;; elscreen
;;=========================================================
(el-get-bundle elscreen
  (elscreen-start))

;;=========================================================
;; projectile
;;=========================================================
;; (el-get-bundle projectile
;;   (require 'projectile)

;;   (custom-set-variables
;;    '(projectile-enable-caching t)		; キャッシュ設定
;;    '(projectile-keymap-prefix (kbd "M-p"))

;;   (setq projectile-globally-ignored-directories
;; 		(append '(
;; 				  ".svn"
;; 				  "out"
;; 				  )
;; 				projectile-globally-ignored-directories))

;;   (setq projectile-globally-ignored-files
;; 		(append '(
;; 				  ".DS_Store"
;; 				  "GTAGS"
;; 				  "GPATH"
;; 				  "GRTAGS"
;; 				  "*.obj"
;; 				  )
;; 				projectile-globally-ignored-files))
;;   )

;; 										;  (when (require 'helm nil t)
;;   ;; helmインストール済み
;;   (el-get-bundle helm-projectile
;; 	(custom-set-variables
;; 	 '(helm-projectile-fuzzy-match nil)
;; 	 '(projectile-completion-system 'helm)
;; 	 )

;; 	(helm-projectile-on)
;; 	)
;;   (projectile-mode t)
;;   )

;;=========================================================
;; flycheck
;;=========================================================
(el-get-bundle flycheck :depends (f)
  ;; Python
  (add-hook 'python-mode-hook 'flycheck-mode)

  ;; Ruby
  (add-hook 'ruby-mode-hook 'flycheck-mode)

  ;; flymake
  ;; (smartrep-define-key global-map "M-g" '(("M-n" . 'flymake-goto-next-error)
  ;; 										  ("M-p" . 'flymake-goto-prev-error)))
  )

;;=========================================================
;; magit
;;=========================================================
;; Windowsだとエラーになる
;;(el-get-bundle magit :branch "2.8.0" :depends (dash with-editor))

;; magitの依存パッケージ
(el-get-bundle dash)
(el-get-bundle with-editor)

;;=========================================================
;; dired-hacks
;;=========================================================
(el-get-bundle dired-hacks :depends (f dash s cl-lib)
  (dired-filter-mode)		; フィルターモードデフォルトON
  )

;;=========================================================
;; idoモード拡張
;;=========================================================
(el-get-bundle ido-vertical-mode)
