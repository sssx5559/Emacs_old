;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; Meadow3.0(Windows用Emacs)設定ファイル
;;-----------------------------------------------------------------------------

;;===============================================
;; apel(汎用ライブラリ)
;;===============================================
(add-to-list 'load-path (concat emacs-dir "meadow/apel"))

;;===============================================
;; elscreen
;;===============================================
(add-to-list 'load-path (concat emacs-dir "meadow/elscreen"))
(require 'elscreen)

;;===============================================
;; anything
;;===============================================
(add-to-list 'load-path (concat emacs-dir "meadow/anything"))

(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
	;; root権限でアクションを実行するときのコマンド
	;; デフォルトは"su"
	(setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  ;; migemo設定側で実施
;  (and (migemop)
;	   (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
	;; M-xによる補完をAnythingで行う
	;(anything-read-string-mode 1)
	;; lispシンボルの補完候補の再検索時間
	(anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
	(require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
	;; describe-bindingsをAnythingに置き換える
	;(descbinds-anything-install)
	)

  (require 'anything-grep nil t)

  ;; (require 'anything-gtags)
  ;; (setq anything-sources
  ;; 		'(anything-c-source-gtags-select))

  (when (require 'anything-project nil t)
	;; ディレクトリを除外する
	 (setq ap:project-files-filters
		   (list
			(lambda (files)
			  (remove-if 'file-directory-p files))))

	(add-hook 'gtags-mode-hook
			  (lambda ()
				(ap:add-project
				 :name 'global
				 :look-for '("GTAGS")
;				 :include-regexp '("\\.c$" "\\.h$" "\\.s$")
;; 				 :exclude-regexp "/out" ; can be regexp or list of regexp
				 :exclude-regexp '("/out" "~$") ; can be regexp or list of regexp
				 ))))

  ;;=========================================================
  ;; anythingで表示する項目
  ;;=========================================================
  (setq anything-sources
		'(
		  anything-c-source-buffers+
		  anything-c-source-recentf
		  anything-c-source-buffer-not-found
		  anything-c-source-files-in-current-dir
		  ;anything-c-source-imenu
		  ))

  ;;=========================================================
  ;; カスタムコマンド
  ;;=========================================================

  ;; manやinfoを調べるコマンド例
  (setq anything-for-document-sources
		(list
		 anything-c-source-man-pages
		 anything-c-source-info-cl
		 anything-c-source-info-pages
		 anything-c-source-info-elisp
		 anything-c-source-apropos-emacs-commands
		 anything-c-source-apropos-emacs-functions
		 anything-c-source-apropos-emacs-variables))

  (defun anything-for-document ()
	"Preconfigured `anything' for anything-for-document."
	(interactive)
	(anything anything-for-document-sources
			  (thing-at-point 'symbol) nil nil nil
			  "*anything for document*"))

  ;; Emacs関数の検索(interactiveではないものも含む)
  (defun anything-for-emacs-functions ()
	"Preconfigured `anything' for anything-for-emacs-functions."
	(interactive)
	(anything (list anything-c-source-emacs-functions)
			  nil nil nil nil
			  "*anything for emacs functions*"))

  ;; imenuの検索
  (defun anything-for-imenu ()
	"Preconfigured `anything' for anything-for-imenu."
	(interactive)
	(anything (list anything-c-source-imenu)
			  nil nil nil nil
			  "*anything for imenu*"))

  ;;=========================================================
  ;; anything関連のキー設定
  ;;=========================================================
  (global-set-key (kbd "C-c k") 'anything-show-kill-ring)
  (global-set-key (kbd "C-c e") 'anything-for-emacs-functions)
  (global-set-key (kbd "C-c f") 'anything-for-imenu)
  (global-set-key (kbd "C-:") 'anything-project)
  (global-set-key (kbd "C-;") 'anything)

  ;; 機能毎のnext, previous
  (define-key anything-map (kbd "C-M-n") 'anything-next-source)
  (define-key anything-map (kbd "C-M-p") 'anything-previous-source)

  ;;=========================================================
  ;; anythingのface設定
  ;;=========================================================
  (set-face-background 'anything-isearch-match "chocolate1")
  )

;;===============================================
;; Ruby
;;===============================================
(add-to-list 'load-path (concat emacs-dir "meadow/ruby"))

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
