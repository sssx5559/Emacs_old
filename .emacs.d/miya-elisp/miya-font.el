;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; フォント設定ファイル
;;-----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meadow2.0用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Meadow2.0-font-set ()

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
			  default-frame-alist))

  ;; ＢＤＦフォント読み込み
										;when (require 'font-setup nil t)
										;  (setq font-setup-bdf-dir "C:/Meadow2/Meadow/site-lisp/intlfonts-1.2.1")
										;  (font-setup))
  )

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
;; NTEmacs(24.4.1)用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun NTEmacs-font-set2 (myfont)
 ;; デフォルト フォント
  (set-face-attribute 'default nil :family myfont :height 110)

  ;; プロポーショナル フォント
  (set-face-attribute 'variable-pitch nil :family myfont :height 110)

  ;; 等幅フォント
  (set-face-attribute 'fixed-pitch nil :family myfont :height 110)

  ;; ツールチップ表示フォント
  (set-face-attribute 'tooltip nil :family myfont :height 90)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((meadow2p) (Meadow2.0-font-set))
 ((meadow3p) (Meadow3.0-font-set))
 ((equal emacs-version "24.4.1") (NTEmacs-font-set2 my-font))
 (t (NTEmacs-font-set my-font)))
