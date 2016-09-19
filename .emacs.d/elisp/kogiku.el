;;; kogiku.el --- reading file and directory names with migemo service

;; Copyright (C) 2004, 2005 Masatake YAMATO and Hideyuki SHIRAI

;; Author: Masatake YAMATO <jet@gyve.org> and
;;         Hideyuki SHIRAI <shirai@meadowy.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 日本語ファイル名の入力をmigemoを使って簡単にするプログラムです。

;; (今は使っていないのですが、)以前windowsでmeadowを使っていたときに、
;; 「デスクトップ(実際には半角)」にぶちまけてあるファイルをfind-fileし
;; ようとすると(仮名漢字変換が必要なため)ファイル名の指定が非常に面倒で
;; あると感じました。"desukutoxtupu"と打鍵してそれを「デスクトップ」へ変換
;; して[return]でやっと指定することができました。

;; 「デスクトップ」の指定をたとえば、"desktop"とか "de"[tab]とかで済めば
;; すばらしいと思いませんか？migemoを使えばこれを実現することができそう
;; です。"desktop"や"de"をmigemoで日本語に展開し、展開済みのパターンを
;; 現在のディレクトリにある全てのファイルの名前と照合すれば、ファイル名
;; を指定("desktop"と「デスクトップ」の一致)したり、補完候補を生成
;; ("de"と「デスクトップ」の一致)したりすることができます。

;; kogiku.elはmigemo.el及びGNU Emacs 21に依存しています。migemo.elの
;; 動作をまず確認して下さい。kogiku.elをロードパスが通って通っている
;; ディレクトリに置き.emacsに (require 'kogiku)と書き加えてemacsを再
;; 起動します。migemoは http://migemo.namazu.org から入手できます。

;;; Acknowledgments:

;; Some code used in `kogiku-file-name-completion'
;; and `kogiku-try-completion-regexp' are copied from
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=kogiku
;; (MeadowMemo maintained by Akihisa Matsushita <akihisa@mail.ne.jp>).

;; `kogiku-read-file-name-internal' is
;; translated from emacs/src/fileio.c::read-file-name-internal
;; in elisp.

;;; History:
;; 2.0.2
;; -- inlucde shirai's patch
;; -- introduce custom
;;
;; 2.0.1 -- update `Acknowledgments'.
;;
;; 2.0 -- rewrite.

;;; Code:

(defun kogiku-version ()
  (interactive)
  (message "小菊 %s" "2.0.2"))

(eval-when-compile
  (require 'cl)
  (require 'migemo))

(defgroup kogiku nil
  "reading file and directory name with migemo service"
  :group 'convenience)

(defcustom kogiku-enable-once t
  "*If non-nil, kogiku effects a one time when call with a prefix argument.
If nil, kogiku toggle with a prefix argument."
  :group 'kogiku
  :type 'boolean)

(defcustom kogiku-mode-change-key "\M-k"
  "*Key of change `kogiku-enable-once'."
  :group 'kogiku
  :type 'sexp)

(defcustom kogiku-take-over-targets '(read-file-name-internal
				      ffap-read-file-or-url-internal)
  "*小菊がのっとる`minibuffer-completion-table'の種類。
minibufferに制御が移ったときに`minibuffer-completion-table'の値が
`kogiku-take-over-targets'の要素であれば、小菊による補完の準備を行なう。"
  :group 'kogiku
  :type '(repeat symbol))

(defcustom kogiku-minibuffer-prompt-string "kogiku"
  "*Kogiku indicator in minibuffer."
  :group 'kogiku
  :type 'string)

(defcustom kogiku-minibuffer-indicator-strings '("ON" "Fire")
  "*Indicators in minibuffer prpmpt."
  :group 'kogiku
  :type '(list (string :tag "Kogiku-mode")
	       (string :tag "Oneshot-mode")))

(defcustom kogiku-use-advocate t
  "*Use kogiku `advocate' mode."
  :group 'kogiku
  :type 'boolean)

(defface kogiku-indicator-face
  '((((class color) (type tty)) (:foreground "blue" :bold t))
    (((class color) (background light)) (:foreground "dark blue" :bold t))
    (((class color) (background dark))  (:foreground "cyan" :bold t))
    (t (:bold t)))
  "*Face of kogiku indicator."
  :group 'kogiku)

(defface kogiku-indicator-advocate-face
  '((((class color) (type tty)) (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "firebrick" :background "wheat" :bold t))
    (((class color) (background dark))
     (:foreground "pink" :background "firebrick" :bold t))
    (t (:inverse-video t :bold t)))
  "*Face of kogiku indicator."
  :group 'kogiku)

(defvar kogiku-original-functions nil)
(defvar kogiku-original-completion-tables nil)

(defvar kogiku-completion-key "\t")
(defvar kogiku-minibuffer-prompt-overlay nil)
(defvar kogiku-mode-change-original-functions nil)

(defvar kogiku-emacs21-p (fboundp 'field-beginning))
(defvar kogiku-minibuffer-prompt-advocate-files nil)
(defvar kogiku-migemo-pattern-alist nil)
(defvar kogiku-migemo-pattern-alist-length 128)

(defvar kogiku-minibuffer-prompt-map nil
  "kogiku prompt map for mode change.")

(let ((map (make-sparse-keymap)))
  (define-key map [mouse-2] 'kogiku-mode-change-at-mouse)
  (setq kogiku-minibuffer-prompt-map map))

(if kogiku-emacs21-p
    (defalias 'kogiku-field-beginning 'field-beginning)
  (defalias 'kogiku-field-beginning 'point-min))

(defun kogiku-complete (&optional arg)
  (interactive "P")
  (let ((minibuffer-completion-table minibuffer-completion-table))
    (when (or (and kogiku-enable-once arg)
	      (not (or kogiku-enable-once arg)))
      (setq minibuffer-completion-table 'kogiku-read-file-name-internal))
    (funcall (car kogiku-original-functions))))

(if (fboundp 'compare-strings)
    (defalias 'kogiku-compare-strings 'compare-strings)
  (defun kogiku-compare-strings (string1 start1 end1 string2 start2 end2)
    "Convenience `compare-strings' for XEmacs."
    (let* ((str1 (substring string1 start1 end1))
	   (str2 (substring string2 start2 end2))
	   (len (min (length str1) (length str2)))
	   (i 0))
      (if (string= str1 str2)
	  t
	(setq i (catch 'ignore
		  (while (< i len)
		    (when (not (eq (aref str1 i) (aref str2 i)))
		      (throw 'ignore i))
		    (setq i (1+ i)))
		  i))
	(1+ i)))))

(defun kogiku-try-completion-regexp (regexp all-list)
  (if (= (length regexp) 0)
      ""
    (substring
	     (car all-list)
	     0
	     (apply 'min
		    (mapcar
		     (lambda (a)
		       (apply 'min
			      (mapcar
			       (lambda (b)
				 (- (abs
				     (kogiku-compare-strings a 0 nil
						      b 0 nil))
				    1))
			       (delete a all-list))))
		     all-list)))))

(defun kogiku-migemo-get-pattern (string)
  (let ((migemo-pattern-alist migemo-pattern-alist)
	(migemo-white-space-regexp " *")
	pattern)
    (let ((case-fold-search nil))
      (while (string-match "[^a-zA-Z]\\([a-z]+\\)" string)
	(setq string
	      (replace-match (capitalize (match-string 1 string)) nil nil string 1))))
    (if (setq pattern (assoc string kogiku-migemo-pattern-alist))
	(prog1
	    (cdr pattern)
	  (setq kogiku-migemo-pattern-alist
		(cons pattern
		      (delete pattern kogiku-migemo-pattern-alist))))
      (prog1
	  (setq pattern (migemo-get-pattern string))
	(setq kogiku-migemo-pattern-alist
	      (cons (cons string pattern) kogiku-migemo-pattern-alist))
	(when (> (length kogiku-migemo-pattern-alist)
		 kogiku-migemo-pattern-alist-length)
	  (setcdr
	   (nthcdr (1- kogiku-migemo-pattern-alist-length) kogiku-migemo-pattern-alist)
	   nil))))))

(defun kogiku-file-name-completion (string dir &optional all)
  (let* ((expanded-string (expand-file-name string dir))
	 (files (directory-files dir))
	 (pattern
	  (if (string-match "/$" expanded-string)
	      ""
	    (concat "^\\("
		    (cond
		     ((string-match "\\Ca$" string)
		      string)
		     ((and (string-match "\\ca+$" string)
			   (< 0 (match-beginning 0)))
		      (concat (substring string 0 (match-beginning 0))
			      "\\("
			      (kogiku-migemo-get-pattern (match-string 0 string))
			      "\\)"))
		     (t
		      (kogiku-migemo-get-pattern (file-name-nondirectory string))))
		    "\\)")))
	 (ignore-pattern (format "\\(%s\\)$"
				 (mapconcat 'regexp-quote completion-ignored-extensions "\\|")))
	 (candidates (delete nil
			     (mapcar
			      (lambda (f)
				(when (string-match pattern f)
				  (when (file-directory-p (expand-file-name f dir))
				    ;; ディレクトリは / で終わるように
				    (setq f (file-name-as-directory f)))
				  (and (not (string-match ignore-pattern f)) f)))
			      files)))
	 count common)
    (if all
	candidates
      (when (and (string= string "")
		 (= (length files) 3))
	(setq candidates
	      (delete-if (lambda (f) (string-match "\\.\\.?/" f))
			 candidates)))
      (setq count (length candidates))
      (cond
       ((eq count 0) nil)
       ((eq count 1) (let ((candidate (car candidates)))
		       (if (file-directory-p candidate)
			   (file-name-as-directory candidate)
			 candidate)))
       (t
	(if (null (delete nil (mapcar (lambda (candidate)
					(string-match "\\Ca" candidate))
				      candidates)))
	    ;; 全ての候補が日本語を含んでいない場合、通常通り
	    ;; `try-completion' を呼んでまかせる。
	    (try-completion string (mapcar 'list candidates))
	  (setq common (kogiku-try-completion-regexp pattern candidates))
	  (if (eq (length common) 0) string common)))))))

(defun kogiku-file-name-all-completions (string dir)
  (kogiku-file-name-completion string dir t))

(defun kogiku-double-dollars (input)
  (let ((ret ""))
    (while (string-match "\\$" input)
      (setq ret (concat ret
			(substring input 0 (match-beginning 0))
			"$$"))
      (setq input (substring input (match-end 0))))
    (concat ret input)))

(defun kogiku-read-file-name-internal (string dir action)
  (block nil
    (unless (boundp 'read-file-name-predicate)
      (setq read-file-name-predicate nil))
    (let ((realdir dir)
	  (name string)
	  (orig-string)
	  (changed 0)
	  (val)
	  (specdir))
      (if (eq 0 (length string))
	  (if (eq action 'lambda)
	      (return nil))
	(setq orig-string string)
	(setq string (substitute-in-file-name string))
	(setq changed (null (string-equal string orig-string)))
	(setq name (file-name-nondirectory string))
	(setq val  (file-name-directory string))
	(if (not (null val))
	    (setq realdir (expand-file-name val realdir))))
      (cond
       ((null action)
	(setq specdir (file-name-directory string))
	(setq val     (kogiku-file-name-completion name realdir))
	(return (if (not (stringp val))
		    (if changed
			(kogiku-double-dollars string)
		      val)
		  (if (not (null specdir))
		      (setq val (concat specdir val)))
		  (kogiku-double-dollars val))))
       ((eq action t)
	(let ((all (kogiku-file-name-all-completions name realdir)))
	  (unless (or (null read-file-name-predicate)
		      (eq read-file-name-predicate 'file-exists-p))
	    (delete-if (lambda (x)
			 (not (funcall read-file-name-predicate x)))
		       all))
	  (return all)))
       ((eq action 'lambda)
	(return (if read-file-name-predicate
		    (funcall read-file-name-predicate string)
		  (file-exists-p string))))))))

(defun kogiku-mode-change-at-mouse (event)
  (interactive "e")
  (save-window-excursion
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (kogiku-mode-change))))

(defun kogiku-mode-change ()
  (interactive)
  (setq kogiku-enable-once (not kogiku-enable-once)))

(defun kogiku-minibuffer-prompt ()
  (when (and (window-minibuffer-p (selected-window))
	     (not (input-pending-p)))
    (let* ((advocate (and kogiku-use-advocate
			  (kogiku-minibuffer-prompt-advocate)))
	   (mode (if kogiku-enable-once
		     (nth 1 kogiku-minibuffer-indicator-strings)
		   (nth 0 kogiku-minibuffer-indicator-strings)))
	   (indicator
	    (cond
	     ((and kogiku-emacs21-p advocate)
	      (format "<%s:%s> " kogiku-minibuffer-prompt-string mode))
	     (kogiku-emacs21-p
	      (format "[%s:%s] " kogiku-minibuffer-prompt-string mode))
	     (advocate
	      (format "%s<%s>: " kogiku-minibuffer-prompt-string mode))
	     (t
	      (format "%s(%s): " kogiku-minibuffer-prompt-string mode))))
	   (max (if kogiku-emacs21-p (1+ (point-min)) (point-max)))
	   (force (and (not kogiku-emacs21-p) (eq (point-min) max))))
      (when force
	(insert " ")
	(goto-char (point-min))
	(setq max (point-max)))
      (when kogiku-emacs21-p
	(add-text-properties 0 (1- (length indicator))
			     `(face ,(if advocate
					 'kogiku-indicator-advocate-face
				       'kogiku-indicator-face))
			     indicator)
	(add-text-properties 0 (length indicator)
			     `(local-map 
			       ,kogiku-minibuffer-prompt-map
			       mouse-face
			       highlight
			       help-echo
			       "mouse-2: ワンショットモード(Fire)か常時モード(ON)かを切り替えます。")
			     indicator))
      (if kogiku-minibuffer-prompt-overlay
	  (move-overlay kogiku-minibuffer-prompt-overlay
			(point-min) max)
	(setq kogiku-minibuffer-prompt-overlay
	      (make-overlay (point-min) max)))
      (overlay-put kogiku-minibuffer-prompt-overlay
		   'before-string indicator)
      (when (not kogiku-emacs21-p)
	(overlay-put kogiku-minibuffer-prompt-overlay
		     'face  (when advocate 'kogiku-indicator-advocate-face)))
      (overlay-put kogiku-minibuffer-prompt-overlay 'evaporate t)
      (when force
	(let ((inhibit-quit t))
	  (sit-for 60)
	  (delete-region (point-min) (point-max))))
      indicator)))

(defun kogiku-minibuffer-prompt-advocate ()
  (when (and (window-minibuffer-p (selected-window))
	     (not (input-pending-p)))
    (let* ((full (buffer-substring-no-properties
		  (kogiku-field-beginning) (point-max)))
	   (dir (or (file-name-directory full) default-directory))
	   (file (or (file-name-nondirectory full) ""))
	   (files (cdr (assoc dir kogiku-minibuffer-prompt-advocate-files)))
	   (count 0)
	   (kcount 0)
	   (case-fold-search completion-ignore-case)
	   fileregex tmpfiles host)
      (catch 'advocate
	(unless (and (not files)
		     (or (not (eq (point) (point-max)))
			 (not dir)
			 (and (string-match "^\\(/[^/]+:\\)\\|\\(//[^/]+/[^/]+\\)" dir)
			      (setq host (concat "^" (regexp-quote (match-string 0 dir))))
			      (not (string-match host default-directory)))))
	  (unless (or (input-pending-p)
		      (not (and (file-exists-p dir) (file-directory-p dir))))
	    (unless files
	      (setq files (directory-files dir nil nil 'nosort))
	      (setq kogiku-minibuffer-prompt-advocate-files
		    (cons (cons dir files) kogiku-minibuffer-prompt-advocate-files)))
	    (if (or (and (string= file "") (setq fileregex "^\\Ca"))
		    (and (string-match "\\Ca$" file)
			 (setq fileregex (concat "^" (regexp-quote file) "\\Ca"))))
		(while (and files (not (input-pending-p)))
		  (when (string-match fileregex (car files))
		    (throw 'advocate t))
		  (setq files (cdr files)))
	      (unless (input-pending-p)
		(setq tmpfiles files)
		(setq fileregex (concat "^" (regexp-quote file) "\\ca"
					"\\|^" (regexp-quote file) "$"))
		(while (and tmpfiles (not (input-pending-p)))
		  (when (string-match fileregex (car tmpfiles))
		    (setq count (1+ count)))
		  (setq tmpfiles (cdr tmpfiles)))
		(unless (input-pending-p)
		  (setq fileregex
			(concat "^\\("
				(if (and (string-match "\\ca+$" file)
					 (< 0 (match-beginning 0)))
				    (concat (substring file 0 (match-beginning 0))
					    "\\("
					    (kogiku-migemo-get-pattern (match-string 0 file))
					    "\\)")
				  (kogiku-migemo-get-pattern file))
				"\\)"))
		  (while (and (<= kcount count)
			      files (not (input-pending-p)))
		    (when (string-match fileregex (car files))
		      (setq kcount (1+ kcount)))
		    (setq files (cdr files)))
		  (> kcount count))))))))))

(defun kogiku-install-key ()
  (when (memq minibuffer-completion-table kogiku-take-over-targets)
    (let ((table (car kogiku-original-completion-tables))
	  (func (lookup-key (current-local-map) kogiku-completion-key))
	  (cfunc (lookup-key (current-local-map) kogiku-mode-change-key)))
      (if (eq func 'kogiku-complete)
	  (progn
	    (push table kogiku-original-completion-tables)
	    (push (car kogiku-original-functions)
		  kogiku-original-functions)
	    (push (car kogiku-mode-change-original-functions)
		  kogiku-mode-change-original-functions))
	(push minibuffer-completion-table kogiku-original-completion-tables)
	(push func kogiku-original-functions)
	(push cfunc kogiku-mode-change-original-functions))
      (define-key (current-local-map) kogiku-completion-key 'kogiku-complete)
      (define-key (current-local-map) kogiku-mode-change-key 'kogiku-mode-change)
      (setq kogiku-minibuffer-prompt-advocate-files nil)
      (add-hook 'post-command-hook 'kogiku-minibuffer-prompt)
      (kogiku-minibuffer-prompt))))

(add-hook 'minibuffer-setup-hook 'kogiku-install-key)

(defun kogiku-uninstall-key ()
  (when (and (or (eq minibuffer-completion-table 'kogiku-read-file-name-internal)
		 (memq minibuffer-completion-table kogiku-take-over-targets))
	     (eq (lookup-key (current-local-map) kogiku-completion-key)
		 'kogiku-complete))
    (pop kogiku-original-completion-tables)
    (define-key (current-local-map) kogiku-completion-key (pop kogiku-original-functions))
    (when (eq (lookup-key (current-local-map) kogiku-mode-change-key)
	      'kogiku-mode-change)
      (define-key (current-local-map) kogiku-mode-change-key
	(pop kogiku-mode-change-original-functions))))
  (setq kogiku-minibuffer-prompt-advocate-files nil)
  (remove-hook 'post-command-hook 'kogiku-minibuffer-prompt))

(add-hook 'minibuffer-exit-hook 'kogiku-uninstall-key)

(provide 'kogiku)
;; kogiku.el ends here
