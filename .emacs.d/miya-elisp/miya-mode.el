;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; 各モード用設定
;;-----------------------------------------------------------------------------

;;=========================================================
;; Lisp mode
;;=========================================================
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map
	       "\M-?" 'lisp-complete-symbol)))

(add-hook 'lisp-interaction-mode-hook
		  (lambda ()
			(define-key lisp-interaction-mode-map
			  "\M-?" 'lisp-complete-symbol)))

(add-hook 'inferior-lisp-mode-hook
		  (lambda ()
			(define-key inferior-lisp-mode-map
			  "\M-?" 'lisp-complete-symbol)
			(define-key inferior-lisp-mode-map
			  "\C-m" 'newline-and-indent)
			(define-key inferior-lisp-mode-map
			  "\C-j" 'comint-send-input)))

;;Elisp簡易ドキュメント
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; スクリプト言語用関数
(defun miya-run-script (script)
  (shell-command-on-region (point-min) (point-max) script))

;;=========================================================
;; Perl mode
;;=========================================================
(add-hook 'perl-mode-hook
		  (lambda ()
			(define-key perl-mode-map "\C-x@"
			  (lambda ()
				(interactive)
				(miya-run-script "/usr/bin/perl -w")))))

;;=========================================================
;; Python mode
;;=========================================================
;; "-*- coding: XXX -*-"箇所のエンコーディングを検索

;; これを入れないと、python-mode時に警告が出る
(setq python-indent-guess-indent-offset nil)

;; (defun search-coding ()
;;     (save-excursion
;; 	  (goto-char (point-min))
;; 	  (if (search-forward-regexp "coding:[ \t]*\\([-_a-zA-Z0-9]+\\)" nil t)
;; 		  (match-string 1))))

(add-hook 'python-mode-hook
		  (lambda ()
;; 			(setq indent-tabs-mode t)
;;			(setq indent-level 4)
;;			(setq python-indent 4)
			;; 			(setq tab-width 4)
;; 			(setq python-indent-guess-indent-offset nil)

;			(when (load "python-pep8")
										;			  (define-key global-map (kbd "C-c p") 'python-pep8))
			(define-key python-mode-map (kbd "C-c C-c") 'comment-region)))
			;; (define-key python-mode-map (kbd "C-x @")
			;;   (lambda ()
			;; 	(interactive)
			;; 	; リージョンのエンコーディングを指定する
			;; 	(let* ((coding (search-coding))
			;; 		   (coding-system-for-write (if coding
			;; 										(intern coding)
			;; 									  'utf-8)))

			;; 	(miya-run-script my-python))))))

;;=========================================================
;; C mode
;;=========================================================
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-set-style "stroustrup")
	    (define-key c++-mode-map "\C-x@" 'compile)
	    (define-key c-mode-map "\C-x@" 'compile)
		(define-key c-mode-map "\M-?" 'complete-symbol)
		(define-key c++-mode-map "\M-?" 'complete-symbol)
;;		(define-key c-mode-map "\C-ci" 'ff-find-other-file)
		;(gtags-mode)
;; 		(hide-ifdef-mode t)
;; 		(hide-ifdefs)
		(setq cpp-config-file "~/.cpp.el")))

;;=========================================================
;; Ruby mode
;;=========================================================
(autoload 'rubydb "rubydb3x" nil t)
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(setq auto-mode-alist
	  (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist
	  (append '(("^#!.*ruby" . ruby-mode)) interpreter-mode-alist))

(add-hook 'ruby-mode-hook
		  (lambda ()
			(define-key ruby-mode-map "\C-x@"
			  (lambda ()
				(interactive)
				(miya-run-script "ruby")))

			(define-key ruby-mode-map "\C-m"
			  'ruby-reindent-then-newline-and-indent)))

;; ruby-electric.el --- electric editing commands for ruby files
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;;;;  flymake for ruby
(when (require 'flymake nil t)
  (set-face-background 'flymake-errline "purple1")
  (set-face-background 'flymake-warnline "dark slate blue")
  ;; Invoke ruby with '-c' to get syntax checking
  (defun flymake-ruby-init ()
	(let* ((temp-file   (flymake-init-create-temp-buffer-copy
						 'flymake-create-temp-inplace))
		   (local-file  (file-relative-name
						 temp-file
						 (file-name-directory buffer-file-name))))
	  (list "ruby" (list "-c" local-file))))
  (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
  (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

  (add-hook 'ruby-mode-hook
			'(lambda ()
			   ;; Don't want flymake mode for ruby regions in rhtml files
			   (if (not (null buffer-file-name)) (flymake-mode))))

  (add-hook 'ruby-mode-hook
			'(lambda ()
			   (define-key ruby-mode-map "\C-cd"
				 'flymake-display-err-menu-for-current-line))))


;;=========================================================
;; Scheme mode
;;=========================================================
;(setq scheme-program-name "guile -l /cygdrive/c/home/Scheme/miya-macro.scm")

(add-hook 'inferior-scheme-mode-hook
	  (lambda ()
	    (define-key inferior-scheme-mode-map "\C-m" 'newline-and-indent)
		(define-key inferior-scheme-mode-map "\C-j" 'comint-send-input)))

(defun miya-run-scheme ()
  (let ((buf  (get-buffer-create "* run-scheme*")))
	(call-process "guile" nil buf nil
				  "-c" (buffer-substring (point-min) (point-max)))
	(set-buffer buf)
	(message (buffer-substring (point-min) (point-max)))
	(kill-buffer buf)))

(add-hook 'scheme-mode-hook
		  (lambda ()
			(define-key scheme-mode-map "\C-x@"
			  (lambda ()
				(interactive)
				(miya-run-scheme)))))

;;=========================================================
;; Common Lisp Mode
;;=========================================================
(setq inferior-lisp-program "clisp")

;; (defun miya-run-clisp ()
;;   (let ((buf  (get-buffer-create "* run-clisp*")))
;; 	(call-process "clisp" nil buf nil
;; 				  "-x" (buffer-substring (point-min) (point-max)))
;; 	(set-buffer buf)
;; 	(message (buffer-substring (point-min) (point-max)))
;; ;; 	(kill-buffer buf)))
;; 	))

;; (defun miya-run-clisp ()
;;   (let ((ret)
;; 		(buf  (get-buffer-create "* run-clisp*")))
;; 	(call-process "clisp" nil buf nil
;; 				  "-x" (buffer-substring (point-min) (point-max)))
;; 	(set-buffer buf)
;; 	(setq ret (buffer-substring (point-min) (point-max)))
;; 	(message (substring ret (string-match "help.*\n" ret)
;; 						(string-match "Bye." ret)))
;; 	(kill-buffer buf)))

(defun miya-run-clisp ()
  (let ((msg)
		(file "temp.lisp")
		(buf  (get-buffer-create "* run-clisp*")))
	(write-region (point-min) (point-max) file nil 1)
	(call-process "clisp" nil buf nil file)
	(set-buffer buf)
 	(message (buffer-substring (1+ (point-min)) 	; 改行が最初に入るので除く
 							   (1- (point-max))))	; 改行が最後に入るので除く
;	(message (buffer-substring (point-min) (point-max)))
	(delete-file file)
	(kill-buffer buf)))

(add-hook 'lisp-mode-hook
		  (lambda ()
			(define-key lisp-mode-map "\C-x@"
			  (lambda ()
				(interactive)
				(miya-run-clisp)))))

;;=========================================================
;;GNU GLOBAL
;;=========================================================
(unless (require 'helm-gtags nil t)
  ;; helm-gtagsがない場合
  (autoload 'gtags-mode "gtags" "" t)

  (setq gtags-mode-hook
		'(lambda ()
										;(local-set-key "\M-t" 'gtags-find-tag)
		   (local-set-key "\C-cr" 'gtags-find-rtag)
		   (local-set-key "\C-cs" 'gtags-find-symbol)
										;(local-set-key "\C-t" 'gtags-pop-stack)
		   (local-set-key "\C-cp" 'gtags-find-pattern)
		   (local-set-key (kbd "C-c t") 'miya-remake-gtags)
		   ))

  (add-hook 'java-mode-hook (lambda () (gtags-mode 1))) 
  (add-hook 'c-mode-hook (lambda () (gtags-mode 1))) 
  (add-hook 'c++-mode-hook (lambda () (gtags-mode 1))))

;;=========================================================
;; Haskell mode
;;=========================================================
(add-to-list 'load-path (concat emacs-dir "haskell-mode-2.3"))
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

(setq haskell-literate-default 'latex)
(setq haskell-doc-idle-delay 0)

;; バッファから直接実行
(defun miya-run-haskell ()
  (let ((ret)
		(file "temp.hs")
		(buf  (get-buffer-create "* run-haskell*")))
	(write-region (point-min) (point-max) file nil 1)
	(call-process "runghc" nil buf nil file)
	(set-buffer buf)
	(setq ret (buffer-substring (point-min) (point-max)))
	(delete-file file)
	(kill-buffer buf)
	; 結果の余分な空白を削除
	(message (substring ret 0 (string-match "[ \n]+$" ret)))))

(add-hook 'haskell-mode-hook
		  (lambda ()
			(define-key haskell-mode-map "\C-x@"
			  (lambda ()
				(interactive)
				(miya-run-haskell)))))

;;=========================================================
;; Scala
;;=========================================================
;; (add-to-list 'load-path "~/.emacs.d/scala-mode")
;; (require 'scala-mode-auto)
;; ;(require 'scala-mode-feature-electric)
;; ;    (add-hook 'scala-mode-hook
;; ;	      (lambda ()
;; ;		(scala-electric-mode)))

;; ;; ENSIME(the ENhanced Scala Interaction Mode for Emacs)
;; ;(add-to-list 'load-path "~/emacs/ensime_2.9.0-1-0.6.1/elisp")
;; ;(require 'ensime)
;; ;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; ;; (defun miya-run-scala ()
;; ;;   (let ((ret)
;; ;; 		(file "temp.scala")
;; ;; 		(buf  (get-buffer-create "* run-scala*")))
;; ;; 	(write-region (point-min) (point-max) file nil 1)
;; ;; 	(call-process "scala" nil buf nil file)
;; ;; 	(set-buffer buf)
;; ;; 	(setq ret (buffer-substring (point-min) (point-max)))
;; ;; 	(delete-file file)
;; ;; 	(kill-buffer buf)
;; ;; 	(message (substring ret 0 (string-match "[ \n]+$" ret)))))

;; ;; 初回起動時は"java"プロセスが2つ起動するので、終了待ちから脱出できなくなる。
;; ;; bashから手動で"scala"コマンドを実行してから起動すると回避できる。(Windows)
;; (defun miya-run-scala ()
;;   (let ((ret)
;; 		(file "temp.scala")
;; 		(buf  (get-buffer-create "* run-scala*")))
;; 	(write-region (point-min) (point-max) file nil 1)
;; 	(call-process "scala" nil buf nil file)
;; ;	(start-process "scala" buf "scala" file)
;; 	(set-buffer buf)
;; 	(setq ret (buffer-substring (point-min) (point-max)))
;; 	(delete-file file)
;; 	(kill-buffer buf)
;; 	(message (substring ret 0 (string-match "[ \n]+$" ret)))))

;; (add-hook 'scala-mode-hook
;; 		  (lambda ()
;; 			(define-key scala-mode-map "\C-x@"
;; 			  (lambda ()
;; 				(interactive)
;; 				(miya-run-scala)))
;; 			(define-key scala-mode-map "\C-m"
;; 			  (lambda ()
;; 				(interactive)
;; 				(progn
;; 				  (scala-newline)
;; 				  (scala-indent-line))))))

;;=========================================================
;; EShell
;;=========================================================
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map "\C-a" 'eshell-bol)))
(setq eshell-prompt-regexp "[#$] ")		; prompt



;;=========================================================
;; VB
;;=========================================================
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

;(setq visual-basic-ide-pathname "H:/VS98/VB98/VB6.EXE")
;(setq vbp-ide-pathname "H:/VS98/VB98/VB6.EXE")

(setq auto-mode-alist 
    (append '(("\\.[Ff][Rr][Mm]$" . visual-basic-mode)  ;;Form Module
			  ("\\.[Bb][Aa][Ss]$" . visual-basic-mode)  ;;Bas Module
			  ("\\.[Cc][Ll][Ss]$" . visual-basic-mode)  ;;Class Module
			  ("\\.[Vv][Bb][Ss]$" . visual-basic-mode)) ;;VBScript file
			auto-mode-alist))

(autoload 'vbp-mode "vbp-mode" "VBP mode." t)

(setq auto-mode-alist 
    (append '(("\\.[Vv][Bb][Pp]$" . vbp-mode) 
			  ("\\.[Vv][Bb][Gg]$" . vbp-mode)) auto-mode-alist))

(defun extension-from-pattern (pattern)
  "Return true extension which is included period from search pattern."
  (let ((beg (string-match "\\." pattern))
		(end (string-match "\\$" pattern)))
    (substring pattern beg end)))

(defun execute-script ()
  (interactive)
  (when (string= (downcase (extension-from-pattern buffer-file-name)) ".vbs")
	(save-buffer)
	(start-process "proc" "proc-buffer" "fiber" buffer-file-name)))

(add-hook 'visual-basic-mode-hook
          '(lambda ()
			 (set-buffer-file-coding-system 'sjis) ;; for japanese string
             (define-key
			   visual-basic-mode-map "\C-x@" 'execute-script)
			 (setq visual-basic-mode-indent 4)))

;;=========================================================
;; Pascal(Delphi)
;;=========================================================
(defun miya-run-pascal ()
  (let* ((ret)
		(src-file (buffer-file-name))
		(exe-file (concat (file-name-sans-extension src-file) ".exe"))
		(buf (get-buffer-create "*run program*")))

	;(write-region (point-min) (point-max) src-file nil 1)
	(basic-save-buffer)
	(call-process "dcc32" nil buf nil src-file) ; compile
	(with-current-buffer buf
	  (goto-char (point-min))
	  (if (search-forward-regexp "エラー" nil t)
		  (message (buffer-string))	; コンパイルエラーメッセージを表示

		; コンパイル正常終了の場合
		(when (file-exists-p exe-file)
		  (delete-region (point-min) (point-max)) ; バッファ文字列削除
		  (call-process (expand-file-name exe-file) nil buf) ; run
		  (message (buffer-string)))))

	(when (file-exists-p exe-file) (delete-file exe-file))
	(kill-buffer buf)
	))

(add-hook 'pascal-mode-hook
		  (lambda ()
			(define-key pascal-mode-map "\C-x@"
			  (lambda () (interactive) (miya-run-pascal)))))

;;=========================================================
;; moccur
;;=========================================================
;; (when (require 'color-moccur nil t)
;;   ;; グローバルマップにoccur-by-moccurを割り当て
;;   ;(define-key global-map (kbd "M-o") 'occur-by-moccur)
;;   ;; スペース区切りでAND検索
;;   (setq moccur-split-word t)
;;   ;; ディレクトリ検索のとき除外するファイル
;;   ;(add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
;;   (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
;;   (require 'moccur-edit nil t)

;;   ;; Migemoを利用できる環境であればMigemoを使う(migemo設定で実施)
;; ;  (when (migemop)
;; ;	(setq moccur-use-migemo t))
;;   )

;; 編集の終了と同時に保存
;; (defadvice moccur-edit-change-file
;;   (after save-after-moccur-edit-buffer activate)
;;   (save-buffer))

;;=========================================================
;; Cygwinパス認識用
;;=========================================================
;(when (and (windowsp)
; 		   (locate-library "cygwin-mount"))
;  (require 'cygwin-mount)
;  (cygwin-mount-activate))

;;=========================================================
;; sdic
;;=========================================================
(add-to-list 'load-path (concat emacs-dir "sdic"))

;; 英和辞書設定
(setq sdic-eiwa-dictionary-list
		(list (list 'sdicf-client (concat emacs-dir "sdic/gene.sdic"))))

;; 和英辞書設定
(setq sdic-waei-dictionary-list
	  (list (list 'sdicf-client (concat emacs-dir "sdic/jedict.sdic"))))

(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;; 見出し語のスタイル
(setq sdic-face-style 'bold) ; デフォルトで bold

;; 見出し語の色
(setq sdic-face-color "hot pink")

;;=========================================================
;; smartchr
;;=========================================================
;; (when (require 'smartchr nil t)
;;  (define-key global-map
;; 	(kbd "=") (smartchr '("=" " = " " == " " === ")))
;;  (define-key global-map
;;    (kbd "&") (smartchr '("&" " & " " && ")))
;;  )

;;=========================================================
;; auto-install
;;=========================================================
;;(install-elisp "http://www.emacswiki.org/emacs/download/auto-install.el")

;; (when (>= emacs-major-version 22)
;;   (defun run-auto-install ()
;; 	(interactive)
;; 	(add-to-list 'load-path (concat emacs-dir "auto-install"))
;; 	(require 'auto-install nil t)
;; 	(setq auto-install-directory (concat emacs-dir "elisp"))
;; 	;; EmacsWiki に登録されている elisp の名前を取得する
;; 	(auto-install-update-emacswiki-package-name t)
;; 	;; 必要であればプロキシの設定を行う
;; 	;;(setq url-proxy-services '(("http" . "proxy.han.shindengen.co.jp:8080")))
;; 	(auto-install-compatibility-setup)
;; 	(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 	))

;;=========================================================
;; multi-term
;;=========================================================
;(when (and (>= emacs-major-version 23)
;		   (linuxp))
;  (require 'multi-term nil t)
;  (setq multi-term-program "/bin/bash"))

;;=========================================================
;; Migemo
;;=========================================================
(when (migemop)
  (interactive)

  ;; cmigemoを使う
  ;; (add-to-list 'load-path (concat emacs-dir "el-get/package/migemo"))
  (require 'migemo)

  (setq migemo-command "cmigemo")
  ;; Migemoのコマンドラインオプション
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; Migemo辞書の場所
  (setq migemo-dictionary my-migemo-dic)

  ;; cmigemoで必須の設定
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)

  ;; キャッシュの設定
  (setq migemo-use-pattern-alist t)	;※この行を有効にすると何故か動かない
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)

  (setq migemo-coding-system 'utf-8)
  ;; Migemoを起動する
  (migemo-init)

  ;; 小菊(find-fileでの日本語名補完)
  (require 'kogiku)
  (kogiku-mode-change)				; デフォルトON

  ;; anything
  ;; (when (featurep 'anything)
  ;; 	(require 'anything-migemo nil t))

  ;; moccur
  ;; (setq moccur-use-migemo t)

  ;; ;; buffer-file-coding-system から言語判別
  ;; ;; unicode も入れた方がいいのかも。
  ;; (defun my-language-check (lang)
  ;;   (let ((coding
  ;; 		 (coding-system-base buffer-file-coding-system)))
  ;; 	(memq
  ;; 	 coding
  ;; 	 (cdr (assoc 'coding-system
  ;; 				 (assoc lang language-info-alist))))))

  ;; ;; 日本語じゃないときは migemo を使わない
  ;; (eval-after-load "migemo"
  ;;   '(progn
  ;; 	 (defadvice isearch-mode
  ;; 	   (before my-migemo-off activate)
  ;; 	   (unless (my-language-check "Japanese")
  ;; 		 (make-local-variable 'migemo-isearch-enable-p)
  ;; 		 (setq migemo-isearch-enable-p nil)))
  ;; 	 (add-hook 'isearch-mode-end-hook
  ;; 			   (lambda ()
  ;; 				 (unless (my-language-check "Japanese")
  ;; 				   (setq migemo-isearch-enable-p t))))))

  (defun migemo-on ()
	(interactive)
	(setq migemo-isearch-enable-p t))

  (defun migemo-off ()
	(interactive)
	(setq migemo-isearch-enable-p nil))

  ;; 終了時に"migemo"を自動終了
  (defadvice save-buffers-kill-emacs (before my-kill-migemo activate)
	(let ((p (get-process "migemo")))
	  (when p
		(set-process-query-on-exit-flag p nil))))
  )

;;=========================================================
;; IME
;;=========================================================
;; Mozc(Google IME)
(when (and (linuxp)
		   (require 'mozc nil t))
  (setq default-input-method "japanese-mozc")
  ;(global-set-key (kbd "C-<zenkaku-hankaku>") 'toggle-input-method))
  (global-set-key (kbd "C-c C-k") 'toggle-input-method))

;;=========================================================
;; imenu
;;=========================================================
(setq imenu-auto-rescan nil)	; 自動でインデックスを作らない

;;=========================================================
;; xdoc2txt
;;=========================================================
(when (windowsp)
  (load-library "xdoc2txt.el"))

;;=========================================================
;; 試行錯誤用ファイル設定
;;=========================================================
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/junk/%Y/%m/%d-%H%M%S.")
  (global-set-key (kbd "C-x C-z") 'open-junk-file)
  )

;;=========================================================
;; 式の評価結果を注釈
;;=========================================================
(when (require 'lispxmp nil t)
    (define-key emacs-lisp-mode-map (kbd "C-x @") 'lispxmp)
	(define-key lisp-interaction-mode-map (kbd "C-x @") 'lispxmp)
	)

;;=========================================================
;; 括弧の対応を保持(Lisp用)
;;=========================================================
;; (when (require 'paredit nil t)
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;;   )

;;=========================================================
;; popwin ※Windows版 24.3.1ではミニバッファ補完がきなくなる
;;=========================================================
;; (when (require 'popwin nil t)
;;   (setq display-buffer-function 'popwin:display-buffer)
;;   )

;;=========================================================
;; ファイル履歴モード
;;=========================================================
(recentf-mode t)
(setq recentf-exclude '("TAGS$"))
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 200)
(require 'recentf-ext)

;;=========================================================
;; 重複バッファ名称
;;=========================================================
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-strip-common-suffix t)

;; 除外するバッファ名
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; 重複していなくても、表示を変える
(setq uniquify-min-dir-content 3)

;;=========================================================
;; ESS
;;=========================================================
;; (add-to-list 'load-path (concat emacs-dir "ess"))
;; (autoload 'R "ess-site" "Call 'R', the 'GNU S' system from the R Foundation.
;; Optional prefix (C-u) allows to set command line arguments, such as
;; --vsize.  This should be OS agnostic.
;; If you have certain command line arguments that should always be passed
;; to R, put them in the variable `inferior-R-args'." t)

;; (require 'ess-site)

;;=========================================================
;; Marmalade(パッケージ管理)
;;=========================================================
;; (when (and (>= emacs-major-version 23)
;; 		   (require 'package nil t))
;;   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;   (package-refresh-contents) ;; list-packagesしなくてもpackage-installできるように
;;   (package-initialize))

;;=========================================================
;; Objective C
;;=========================================================
;; (defun miya-run-objc ()
;;   (let ((ret)
;; 		(file "temp.m")
;; 		(buf  (get-buffer-create "* run-objc*")))
;; 	(write-region (point-min) (point-max) file nil 1)
;; 	(call-process "gcc" nil buf nil file)
;; ;	(start-process "gcc" buf "gcc" file)
;; 	(set-buffer buf)
;; 	(setq ret (buffer-substring (point-min) (point-max)))
;; 	(delete-file file)
;; 	(kill-buffer buf)
;; 	(message (substring ret 0 (string-match "[ \n]+$" ret)))))


;; (add-hook 'objc-mode-hook
;; 		  (lambda ()
;; 			(define-key objc-mode-map "\C-x@"
;; 			  (lambda ()
;; 				(interactive)
;; 				(miya-run-objc)))))

;;=========================================================
;; magit
;;=========================================================
(when (>= emacs-major-version 24)
  ;; 依存パッケージ
;;  (require 'dash)
;;  (require 'with-editor)

  (add-to-list 'load-path (concat emacs-dir "elisp/magit-2.8.0/lisp"))
  (require 'magit)

  (with-eval-after-load 'info
	(info-initialize)
	(add-to-list 'Info-directory-list
				 (concat emacs-dir "magit/Documentation/")))


  (setq-default magit-auto-revert-mode nil)
  (setq vc-handled-backends '())
  (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
  (global-set-key (kbd "C-x m") 'magit-status)
  (global-set-key (kbd "C-c l") 'magit-blame)

  (custom-set-faces
   '(magit-diff-added ((t (:background "black" :foreground "green"))))
   '(magit-diff-added-highlight ((t (:background "white" :foreground "green"))))
   '(magit-diff-removed ((t (:background "black" :foreground "blue"))))
   '(magit-diff-removed-hightlight ((t (:background "white" :foreground "blue"))))
   '(magit-hash ((t (:foreground "red"))))
   )

  ;; 文字コードは、UTF-8で固定
  (add-to-list 'process-coding-system-alist '("git" utf-8 . utf-8))
  (add-hook 'git-commit-mode-hook
          '(lambda ()
             (set-buffer-file-coding-system 'utf-8)))
  )
