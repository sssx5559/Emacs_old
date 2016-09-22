;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; キーバインド設定ファイル
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------
;; global設定
;;---------------------------------------------------------

;;; C-h を backspace として使う。
; (keyboard-translate ?\C-h ?\C-?)
; (global-set-key "\C-h" nil)
(global-set-key "\C-h" 'backward-delete-char)

;; C-chをヘルプとして使う
(global-set-key "\C-ch" 'help-command)

;; 改行キーでオートインデント
(global-set-key "\C-m" 'newline-and-indent)

;; 改行キーの割り当て
(global-set-key "\C-j" 'newline)

;; リージョンコメントアウト
(global-set-key "\C-c\C-c" 'comment-region)

;; リージョンコメント削除
(global-set-key "\C-c\C-u" 'uncomment-region)

;; レジスタリスト
(global-set-key "\C-c\C-r" 'view-register)

;;行ジャンプ
(global-set-key "\C-cj" 'goto-line)

;; quickrun
(global-set-key "\C-x@" 'quickrun)

;; 1行コメント削除
;(global-set-key (kbd "C-c C-k") 'kill-comment)

;; URLからブラウザを表示
;(global-set-key "\C-cl" ;(if (fboundp 'w3m)
;;;;			    'w3m-browse-url
;			  'browse-url-at-point)

;; カーソル上の単語選択
(global-set-key (kbd "C-c @") 'miya-mark-word)

;; フレーム最大化トグル切り替え
(global-set-key (kbd "C-c m") 'miya-toggle-frame-size)

;; フレーム位置変更
(global-set-key (kbd "C-c C-m") 'miya-remove-frame)

;; ファイル履歴モード
(global-set-key (kbd "C-c o") 'recentf-open-files)

;; バッファとwindowを削除
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; ファイル再読み込み

;(global-set-key (kbd "C-c r") '(lambda ()
;								 (interactive)
;								 (revert-buffer nil t)))

;; リスト5 最後のマークに移動
(defun move-to-mark ()
  (interactive)
  (let ((pos (point)))
	(goto-char (mark))
	(push-mark pos)))
(global-set-key "\M-p" 'move-to-mark)

;; リスト6 ウィンドウ内のカーソル移動
(global-set-key (kbd "C-M-h") (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-M-m") (lambda () (interactive) (move-to-window-line nil)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (move-to-window-line -1)))
