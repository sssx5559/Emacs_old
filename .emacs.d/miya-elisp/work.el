;;;; -*- coding: utf-8 -*-

;;=========================================================
;; 勤務表用
;;=========================================================
(global-set-key (kbd "<f12>") 'nippo-edit)
(global-set-key (kbd "<f11>") 'open-worksheet)


;;=========================================================
;; 日報編集
;;=========================================================
(require 'em-glob)		;; For 'eshell-extended-glob'

(defun nippo-edit ()
  (interactive)
  (let* ((nippo-dir (expand-file-name "xxx/勤怠/社内用/日報/"))
		 (time (current-time))
		 (month-dir (concat nippo-dir
							(format-time-string "%m月" time)))
		 (nippo (concat month-dir (format-time-string "/xxx_%Y%m%d_日報.txt" time)))
		 (file-list nil)
		 (as-list nil)
		 (buf nil))

	;; ディレクトリがないなら作成
	(unless (member month-dir (directory-files nippo-dir t))
	  (make-directory month-dir))

	(unless (file-exists-p nippo)
	  ;; 日報がないなら作成する

	  ;; *.txtファイルをサブフォルダを含めて取得
	  (setq file-list (eshell-extended-glob (concat nippo-dir "**/xxx*.txt")))
	  (unless (listp file-list)
		(error "Error:コピー元の日報が見つかりませんでした。"))

	  ;; (更新時刻 . ファイル名)の連想リスト作成
	  (setq as-list (mapcar '(lambda (f)
							   (cons (miya:uptime-to-float f) f)) file-list))

	  ;; debug
;; 	  (let ((debug-buf (get-buffer-create "*debug*")))
;; 			  (with-current-buffer debug-buf
;; 				(dolist (v as-list)
;; 				  (goto-char (point-min))
;; 				  (insert (format "%S\n" v))
;; 				  )))

	  ;; 日付部分を書き換えて新規ファイルで保存
	  (with-temp-buffer
		;; 更新時刻が直近のファイル内容を挿入
		(insert-file-contents
		 (cdr (assoc (apply 'max (mapcar 'car as-list)) as-list)))

		(goto-char (point-min))
		(while (re-search-forward "[0-9]+年[0-9]+月[0-9]+日" nil t)
		  (replace-match (format-time-string "%Y年%m月%d日")))
		(write-region (point-min) (point-max) nippo)))
	(find-file nippo)))

;; ファイル更新時刻を浮動小数点に変換
(defun miya:uptime-to-float (file)
  (let ((time (nth 5 (file-attributes file))))
	(if time
		(+ (first time) (/ (second time) 10000.0))
	  0.0)))

;;=========================================================
;; 日報送信
;;=========================================================
(defun nippo-send ()
  (interactive)
  (let ((buf (generate-new-buffer "*nippo send*"))
		(path "D:/xxx/")
		(old-dir default-directory))
	(with-current-buffer buf
	  (cd path)
	  (call-process "ruby" nil buf nil (concat path "OpMailSend.rb"))
	  (cd old-dir)
	  (message "%s" (buffer-string)))
	(kill-buffer buf)))

;;=========================================================
;; time型チェック
;;=========================================================
  (if (>= emacs-major-version 24)
	  ;; 新Ver
	  (defun miya:timep (time)
		(and (listp time) (= 4 (length time))))
	;; 旧Ver
	  (defun miya:timep (time)
		(and (listp time) (= 3 (length time))))
	  )

;;=========================================================
;; 秒単位の加減算
;;=========================================================
(if (>= emacs-major-version 24)
	;; 新Ver
	(defun miya:add-second (num &optional time)
	  (or time (setq time (current-time)))
	  (let ((quotient)
			(remainder)
			(work))
		(when (and (numberp num)
				   (miya:timep time))
		  (setq work (cadr time))
		  (if (> num (- #xFFFFFFF work))
			  (error "28bit長を超えています。"))
		  (setq work (+ work num))
		  (cond ((>= work #x10000)		; #x10000以上の場合
				 (setq quotient (/ work #x10000))
				 (setq remainder (% work #x10000))
				 (setq time (list (+ quotient (car time)) remainder (nth 2 time) (nth 3 time))))
				((< work 0)				; 0未満の場合
				 (setq quotient (1- (/ work #x10000)))
				 (setq remainder (% work #x10000))
				 (setq time (list (+ quotient (car time)) (+ #x10000 remainder) (nth 2 time) (nth 3 time))))
				(t 						; その他
				 (setcdr time (cons (+ (cadr time) num) (cddr time))))))
		time))

  ;; 旧Ver
  (defun miya:add-second (num &optional time)
	(or time (setq time (current-time)))
	(let ((quotient)
		  (remainder)
		  (work))
	  (when (and (numberp num)
				 (miya:timep time))
		(setq work (cadr time))
		(if (> num (- #xFFFFFFF work))
			(error "28bit長を超えています。"))
		(setq work (+ work num))
		(cond ((>= work #x10000)		; #x10000以上の場合
			   (setq quotient (/ work #x10000))
			   (setq remainder (% work #x10000))
			   (setq time (list (+ quotient (car time)) remainder (third time))))
			  ((< work 0)				; 0未満の場合
			   (setq quotient (1- (/ work #x10000)))
			   (setq remainder (% work #x10000))
			   (setq time (list (+ quotient (car time)) (+ #x10000 remainder) (third time))))
			  (t 						; その他
			   (setcdr time (cons (+ (cadr time) num) (cddr time))))))
	  time))
  )

;;=========================================================
;; 分単位の加減算
;;=========================================================
(defun miya:add-minute (num &optional time)
  (if (> num (/ #xFFFFFFF 60))
	  (error "28bit長を超えています。"))
  (miya:add-second (* num 60)))

;;=========================================================
;; 時単位の加減算
;;=========================================================
(defun miya:add-hour (num &optional time)
  (if (> num (/ #xFFFFFFF 3600))
	  (error "28bit長を超えています。"))
  (miya:add-second (* num 3600)))

;;=========================================================
;; 日単位の加減算
;;=========================================================
(defun miya:add-day (num &optional time)
  (if (> num (/ #xFFFFFFF 86400))
	  (error "28bit長を超えています。"))
  (miya:add-second (* num 86400)))

;; 今月の社内勤務表のパスを返す
(defun get-worksheet ()
  (let ((file (format-time-string
			   "D:/xxx.xlsm")))
	file))

;; 今月のSD勤務表のパスを返す
(defun get-worksheet-sd ()
  (let ((file (format-time-string
			   "D:/xxx.xls")))
	file))

;; 今週の週報のパスを返す
(defun get-weekly-sheet ()
  (let* ((diff (- 6 (string-to-int (format-time-string "%u")))) ; 土曜日(6)までの数値
		 (saturday (miya:add-day diff))
		 (sunday (miya:add-day (- diff 6)))
		 (dir (concat (format-time-string "%Y%m%d" sunday) "-"
					  (format-time-string "%m%d" saturday) "/"))
		 (file (format-time-string "xxx.xls" saturday))
		 (path (concat "D:/xxx/週報/"
					   dir
					   file)))
	path))
;; (defun get-weekly-sheet ()
;;   (let* ((diff (- 6 (string-to-int (format-time-string "%u")))) ; 土曜日(6)までの数値
;; 		 (time (miya:add-day diff))
;; 		 (file (format-time-string
;; 				"C:/xxx.xls" time)))
;; 	file))

;; 勤務表、社内勤務表、週報を選択する
(defun open-worksheet ()
  (interactive)
  (let ((one (get-worksheet))
		(two (get-worksheet-sd))
		(three (get-weekly-sheet))
		(file))
	(setq file (substring
				(completing-read "Select file: "
								 (list
								  (concat "1:" one)
								  (concat "2:" two)
								  (concat "3:" three)) nil t)
				2))
	(open-file-dwim file)))
(put 'dired-find-alternate-file 'disabled nil)
