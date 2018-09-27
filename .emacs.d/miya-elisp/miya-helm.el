;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; Helm設定ファイル
;;-----------------------------------------------------------------------------
(require 'helm-config)
(require 'helm-ag)
(require 'helm-descbinds)
(require 'helm-swoop)

(helm-descbinds-mode)

(when (migemop)
  (helm-migemo-mode))

(helm-mode 1)	;; 既存コマンドをHelmインターフェースに置き換え

;;; helm-mode有効のときでも、処理を変更したいコマンドをリストに登録
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired . nil))
(add-to-list 'helm-completing-read-handlers-alist '(ediff . nil))
(add-to-list 'helm-completing-read-handlers-alist '(ediff-directories . nil))

(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates (lambda ()
                  (let ((cmds))
                    (mapatoms
                     (lambda (elt) (when (commandp elt) (push elt cmds))))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "A simple helm source for Emacs commands.")

(defvar helm-source-emacs-commands-history
  (helm-build-sync-source "Emacs commands history"
    :candidates (lambda ()
                  (let ((cmds))
                    (dolist (elem extended-command-history)
                      (push (intern elem) cmds))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "Emacs commands history")

;; カスタム変数設定
(custom-set-variables
 ;; helm-mini表示カスタマイズ
 '(helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-files-in-current-dir
;                               helm-source-emacs-commands-history
;                               helm-source-emacs-commands
                               ))
 '(helm-truncate-lines t)	; 検索結果を改行しないで、1行表示

 (when (macp)
   '(helm-locate-command "mdfind %s %s") ; SpotLight検索
   )
 )

;;=========================================================
;; helm-ag
;;=========================================================
(when (require 'helm-ag nil t)
  (custom-set-variables
   ;; '(helm-ag-base-command "rg --vimgrep --no-heading")	; ag以外の検索コマンドも使える
   '(helm-ag-ignore-patterns '("*~" "#*#"))
   ;; '(helm-ag-insert-at-point 'word)		; カーソル以下のオブジェクトを検索パターンとして自動挿入
   )

  (when (require 'helm-project nil t)
	;; helm-projectを利用してルートディレクトリから検索
	(defun miya/helm-ag-project-root ()
	  (interactive)
	  (helm-ag (car (hp:get-root-directory)))))
  )

;;=========================================================
;; helm-gtags
;;=========================================================
(when (require 'helm-gtags nil t)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)

  ;; タグから検索を行う際, 大文字小文字を無視する
  ;(setq helm-gtags-ignore-case t)

  ;; ジャンプ直後に、行がずれてチカチカする場合があるので禁止
  (setq helm-gtags-pulse-at-cursor nil)

  ;; key bindings
  (add-hook 'helm-gtags-mode-hook
			'(lambda ()
			   (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
			   (local-set-key (kbd "C-c r") 'helm-gtags-find-rtag)
			   (local-set-key (kbd "C-c p") 'helm-gtags-find-pattern)
			   (local-set-key (kbd "C-c s") 'helm-gtags-find-symbol)
			   (local-set-key (kbd "M-*") 'helm-gtags-pop-stack)
;			   (local-set-key (kbd "C-c C-r") 'helm-gtags-resume)
			   (local-set-key (kbd "C-c t") 'miya-remake-gtags))))

;;=========================================================
;; helm-project
;;=========================================================
;; (when (require 'helm-project nil t)
;;   ;; ディレクトリを除外する
;;   (setq hp:project-files-filters
;; 		(list
;; 		 (lambda (files)
;; 		   (remove-if 'file-directory-p files))))

;;   (add-hook 'helm-gtags-mode-hook
;; 			(lambda ()
;; 			  (hp:add-project
;; 			   :name 'global
;; 			   :look-for '("GTAGS")
;; 			   ;;				 :include-regexp '("\\.c$" "\\.h$" "\\.s$")
;; 			   ;; 				 :exclude-regexp "/out" ; can be regexp or list of regexp
;; 			   :exclude-regexp '("/out" "~$") ; can be regexp or list of regexp
;; 			   ))))

;;=========================================================
;; helmでドキュメント検索
;;=========================================================
(require 'helm-elisp)
(require 'helm-man)

;; 基本ソースを定義
(setq helm-for-document-sources
	  '(helm-source-info-elisp
		helm-source-info-cl
		helm-source-info-pages
		helm-source-man-pages))

;; コマンド定義
(defun helm-for-document ()
  "Preconfigured `helm` for helm-for-document."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
	(helm :sources
		  (nconc
		   (mapcar (lambda (func)
					 (funcall func default))
				   helm-apropos-function-list)
		   helm-for-document-sources)
		  :buffer "*helm for document")))

;;=========================================================
;; helm関連のキー設定
;;=========================================================
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-]") 'helm-swoop)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c b") 'helm-descbinds)
(global-set-key (kbd "C-c o") 'helm-recentf)
(global-set-key (kbd "M-x") 'helm-M-x)
;(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-M-;") 'helm-resume)
(global-set-key (kbd "C-c I") 'helm-imenu)
(global-set-key (kbd "C-c i") 'helm-imenu-in-all-buffers)
;(global-set-key (kbd "C-c C-r") 'helm-resume)
(global-set-key (kbd "C-c a") 'helm-ag)
;(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x r l") 'helm-bookmarks)
(global-set-key (kbd "C-M-g") 'helm-ghq)
(global-set-key (kbd "M-d") 'helm-for-document)

;; projectile関連
(global-set-key (kbd "C-:") 'helm-projectile-find-file)
;(global-set-key (kbd "C-:") 'helm-project)

;; helm-mini中
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "M-*") 'helm-keyboard-quit)
(define-key helm-map (kbd "C-<up>") 'helm-scroll-other-window-down)
(define-key helm-map (kbd "C-<down>") 'helm-scroll-other-window)

;; ミニバッファでC-hをバックスペースに割り当て
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;;(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
;;(define-key helm-map (kbd "C-s") 'helm-next-line)
;;(define-key helm-map (kbd "C-r") 'helm-previous-line)

;; TABキー割り当て
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; helm-swoop
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
