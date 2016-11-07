;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; フォント設定ファイル
;;-----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meadow3.0用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Meadow3.0-font-set ()
  ;;未確定文字のフォント設定
  (setq default-frame-alist
		(cons '(ime-font . (w32-logfont "ＭＳ 明朝"
										0 16 400 0 nil nil nil 128 1 3 17))
			  default-frame-alist))

  (w32-add-font
   "tt-font"
   '((spec
	  ((:char-spec ascii :height 120)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 400 0 nil nil nil 0 1 3 49))
	  ((:char-spec ascii :height 120 :weight bold)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 700 0 nil nil nil 0 1 3 49))
	  ((:char-spec ascii :height 120 :slant italic)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 400 0   t nil nil 0 1 3 49))
	  ((:char-spec ascii :height 120 :weight bold :slant italic)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 700 0   t nil nil 0 1 3 49))
	  ((:char-spec japanese-jisx0208 :height 120)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 400 0 nil nil nil 128 1 3 49))
	  ((:char-spec japanese-jisx0208 :height 120 :weight bold)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 700 0 nil nil nil 128 1 3 49)
	   ((spacing . -1)))
	  ((:char-spec japanese-jisx0208 :height 120 :slant italic)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 400 0   t nil nil 128 1 3 49))
	  ((:char-spec japanese-jisx0208 :height 120 :weight bold :slant italic)
	   strict
	   (w32-logfont "ＭＳ 明朝" 0 -16 700 0   t nil nil 128 1 3 49)
	   ((spacing . -1))))))

  (set-face-attribute 'variable-pitch nil :family "tt-font")

  ;; フレームフォントの設定
  (setq default-frame-alist
		(cons '(font . "tt-font")
			  default-frame-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NTEmacs, Ubuntu用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun NTEmacs-font-set (myfont)
  (set-default-font myfont)

  ;; 固定等幅フォント
  (set-face-attribute 'fixed-pitch    nil :family myfont)
  ;; 可変幅フォント
  (set-face-attribute 'variable-pitch nil :family myfont)
  (add-to-list 'default-frame-alist (cons 'font myfont))
  (set-face-font 'font-lock-comment-face       myfont)
  (set-face-font 'font-lock-string-face        myfont)
  (set-face-font 'font-lock-keyword-face       myfont)
  (set-face-font 'font-lock-builtin-face       myfont)
  (set-face-font 'font-lock-function-name-face myfont)
  (set-face-font 'font-lock-variable-name-face myfont)
  (set-face-font 'font-lock-type-face          myfont)
  (set-face-font 'font-lock-constant-face      myfont)
  (set-face-font 'font-lock-warning-face       myfont))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((meadowp) (Meadow3.0-font-set))
 (t (NTEmacs-font-set my-font)))
