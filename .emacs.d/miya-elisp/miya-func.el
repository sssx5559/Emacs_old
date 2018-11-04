;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; 自作関数設定ファイル
;;-----------------------------------------------------------------------------

;; 2003/05/19 author miyazaki


;; ディレクトリ(サブディレクトリも含む)からファイルを検索する
;; 検索結果は別バッファに表示する
;; 非正規表現バージョン
;;
(defun recursive-find-file (dir file)
  (interactive "DDirectory: \nsSearch file: ")
  (recursive-find-file-main dir file nil))



;; ディレクトリ(サブディレクトリも含む)からファイルを検索する
;; 検索結果は別バッファに表示する
;; 正規表現バージョン
;;
(defun recursive-find-file-regex (dir file)
  (interactive "DDirectory: \nsSearch file: ")
  (recursive-find-file-main dir file t))



;; フラグにより正規表現の有無を振り分ける
;; メイン処理関数
(defun recursive-find-file-main (dir file regex)
  "再帰的にファイルを検索する"
;  (interactive "DDirectory: \nsSearch file: ")
  (let (filelist tmpbuf tmp)
    (setq filelist (my-search-dirs dir file regex))
    (if (null filelist)
        (message "無いぴょん")
      (setq tmpbuf (get-buffer-create "*File List*"))
      (pop-to-buffer tmpbuf)
      (erase-buffer)
      (insert "Э検索結果Э\n\n")
	  (insert "★操作方法  [上移動:p  下移動:n  終了:q\n")
	  (insert "             ファイルオープン:o   ファイルオープン(全画面):O ]\n\n")
      (while (setq tmp (car filelist))
        (insert tmp "\n")
        (setq filelist (cdr filelist)))
	  (goto-line 5)
	  (message "検索結果")
	  (use-local-map recursive-find-file-map)
	  (setq major-mode 'recursive-find-file-mode
			mode-name "検索結果モード"))))



;; ディレクトリから再帰的にファイルを検索し、リストを生成する(検索関数) 
;; 
;; 引数：ディレクトリ、検索ファイル名、正規表現フラグ
;;
;; 戻り値：検索に成功したファイル名(絶対パス)のリスト
;;         検索に失敗した場合はnil

(defun my-search-dirs (dir target regex)
  ; ディレクトリ名が "/"で終わっている場合取り除く
  (if (string-match "\\/$" dir)
      (setq dir (substring dir 0 (1- (length dir)))))
  (message "検索中・・・")
  ;ディレクトリエントリからカレントディレクトリ、親ディレクトリを除き、
  ;絶対パスに変換する 
  (let ((entry (mapcar '(lambda (arg) (concat dir "/" arg))
					  (delete "."
							  (delete ".."
									  (directory-files dir)))))
		filelist
		result)
	(while (setq result (car entry))
	  (cond 
        ;ディレクトリならば再帰呼び出し
	   ((file-directory-p result)
		(setq result (my-search-dirs result target regex))
		(if result (setq filelist (append result filelist))))

       ;正規表現で検索
	   (regex
		(if (string-match target (my-change-path result))
		;検索ファイルならばリストに追加
			(setq filelist (cons result filelist))))

	   ;非正規表現で検索
	   ((string= (concat dir "/" target) result)
		;検索ファイルならばリストに追加
		(setq filelist (cons result filelist))))

	  (setq entry (cdr entry)))
	filelist))

;; 絶対パスから親ディレクトリを除く
;; 例 c:/aaa/bbb/ccc → ccc
(defun my-change-path (path)
  (let (index)
	(if (setq index (string-match "/" path))
		(my-change-path (substring path (1+ index)))
	  path)))


;; カレント行のファイル名を取得
(defun my-get-filename ()
  (save-excursion
	(beginning-of-line)
	(buffer-substring (point)
					  (progn
						(skip-chars-forward "^ \t\n")
						(point)))))

;; カレント行のファイルをオープンする(半画面)
(defun my-half-open-file ()
  (interactive)
  (let ((filename (my-get-filename)))
		(if (and (file-exists-p filename)
				 (/= 0 (length filename)))
			(find-file filename))))


;; カレント行のファイルをオープンする(全画面)
(defun my-full-open-file ()
  (interactive)
  (let ((filename (my-get-filename)))
	(if (and (file-exists-p filename)
			 (/= 0 (length filename)))
		(progn
		  (find-file filename)
		  (delete-other-windows)))))

;; バッファとウインドウを閉じる
(defun my-end-buffer ()
  (interactive)
  (if (get-buffer "*File List*")
	  (progn
		(kill-buffer (get-buffer "tmp"))
		(if (null (one-window-p))
			(delete-window)))))


;; 上に進む
(defun my-move-up (arg)
  (interactive "p")
  (forward-line (- arg)))

;; 下に進む
(defun my-move-down (arg)
  (interactive "p")
  (forward-line  arg))


;; 検索結果バッファ用キーマップ
(defvar recursive-find-file-map (make-keymap))
(define-key recursive-find-file-map "n" 'my-move-down)
(define-key recursive-find-file-map "p" 'my-move-up)
(define-key recursive-find-file-map "o" 'my-half-open-file)
(define-key recursive-find-file-map "O" 'my-full-open-file)
(define-key recursive-find-file-map "q" 'my-end-buffer)


(defun miya-delete-except-region ()
  "リージョン以外の部分を削除する"
  (interactive)
  (save-excursion
	(delete-region (point-min) (region-beginning))
	(delete-region (region-end) (point-max))))
  ; リージョンの再設定
;  (set-mark (point-min))
;  (goto-char (point-max)))

;; (count-lines-region START END)で実装されている(M-=)
;;
;; (defun miya-region-count-chars ()
;;   "リージョン内の文字数をカウントする"
;;   (interactive)
;;   (message (format "%s" (- (region-end) (region-beginning)))))

(defun miya-region-count-lines (&optional start end)
  "リージョン内の行数をカウントする"
  (if (or (null start) (null end))
	  (setq start (region-end)
			end (region-beginning)))
  (let ((count 0))
	(save-excursion
	  (goto-char start)
	  (while (< (point) end)
		(setq count (1+ count))
		(forward-line 1)))
	count))


;; エラーシンボル一覧ファイル作成 2004/3/31
(defun miya-make-error-conditions-file (filename)
  "エラーシンボル一覧ファイル作成"
  (interactive "sFile Name: ")
  (let ((buf (get-buffer-create " *miya-make-error-conditions-file*")))
	(set-buffer buf)
	(erase-buffer)
	(goto-char (point-min))
	(mapatoms (lambda (atom)
				(if (get atom 'error-conditions)
					(insert (format "%s\t%s\t\"%s\"\n"
									atom
									(get atom 'error-conditions)
									(get atom 'error-message))))))
	(sort-lines nil (point-min) (point-max))
	(write-file filename)
	(kill-buffer buf)))


;; 正規表現を満たすバッファを削除	2004/4/1
(defun miya-kill-some-buffer (regex)
  "正規表現を満たすバッファを削除"
  (interactive "sRegex: ")
  (let ((buflist (mapcar (lambda (buf)
						   (if (string-match regex (buffer-name buf))
							   buf))
						 (buffer-list))))
	(setq buflist (delq nil buflist))
	(if buflist
		(kill-some-buffers buflist)
	  (message "Not Found"))))


;; フレームサイズ変更
(defun miya-resize-frame (size)
  "フレームサイズ変更

[key] : [action]
q : 終了
c : 元のサイズに戻す
f : 幅+
b : 幅-
n : 高さ+
p : 高さ-"
  (interactive "p")
  (catch 'exit
	(let ((init-height (frame-height))
		  (init-width (frame-width))
		  (key))
	(while t
	  (message "q:Quit c:Cancel [fbnp]:Resize  height = %d width = %d"
			   (frame-height) (frame-width))
	  (setq key (read-char))
	  (cond
	   ((eq key ?p) (set-frame-height nil (- (frame-height) size)))
	   ((eq key ?n) (set-frame-height nil (+ (frame-height) size)))
	   ((eq key ?b) (set-frame-width nil (- (frame-width) size)))
	   ((eq key ?f) (set-frame-width nil (+ (frame-width) size)))
	   ((eq key ?c)
		(set-frame-height nil init-height)
		(set-frame-width nil init-width))
	   ((eq key ?q) (throw 'exit t)))))))


;; フレーム位置変更
(defun miya-remove-frame (pos)
  "フレーム位置変更

[key] : [action]
q : 終了
c : 元の位置に戻す
f : 左端+
b : 左端-
n : 上端+
p : 上端-"  
  (interactive "p")
;  (if (< pos 10) (setq pos 20))
  (if (= pos 1) (setq pos 40))
  (catch 'exit
	(let* ((init-xpos (cdr (assq 'left (frame-parameters))))
		   (init-ypos (cdr (assq 'top (frame-parameters))))
		   (xpos init-xpos) 
		   (ypos init-ypos)
		   (key))
	  (while t
		(message "q:Quit c:Cancel [fbnp]:Remove  top = %d left = %d" ypos xpos)
		(setq key (read-char))
		(cond
		 ((eq key ?p) (setq ypos (- ypos pos)))
		 ((eq key ?n) (setq ypos (+ ypos pos)))
		 ((eq key ?b) (setq xpos (- xpos pos)))
		 ((eq key ?f) (setq xpos (+ xpos pos)))
		 ((eq key ?c) (setq ypos init-ypos
							xpos init-xpos))
		 ((eq key ?q) (throw 'exit t)))
		 (set-frame-position (selected-frame) xpos ypos)))))

;; フレーム初期化
(defun miya-init-frame ()
  (interactive)
  (progn
	(set-frame-height nil my-screen-height)
	(set-frame-width nil my-screen-width)
	(set-frame-position (selected-frame) my-screen-left my-screen-top)))

;; リージョン内の全角カタカナを全角ひらがなに変換
(defun miya-region-zenkaku-katakana-to-zenkaku-hiragana (start end)
  "リージョン内の全角カタカナを全角ひらがなに変換"
  (interactive "r")
  (miya-common-convert start end (miya-make-zenkaku-table) nil))
;;   (save-excursion
;; 	(goto-char start)
;; 	(let ((mark-active nil) (char))
;; 	  (while (search-forward-regexp "[ア-ン]" end t)
;; 			(progn
;; 			  (setq char (char-before))
;; 			  (backward-delete-char 1)
;; 			  (insert-char (- char (- ?ア ?あ)) 1))))))


;; リージョン内の全角ひらがなを全角カタカナに変換
(defun miya-region-zenkaku-hiragana-to-zenkaku-katakana (start end)
  "リージョン内の全角ひらがなを全角カタカナに変換"
  (interactive "r")
  (miya-common-convert start end (miya-make-zenkaku-table) t))
;;   (save-excursion
;; 	(goto-char start)
;; 	(let ((mark-active nil) (char))
;; 	  (while (search-forward-regexp "[あ-ん]" end t)
;; 			(progn
;; 			  (setq char (char-before))
;; 			  (backward-delete-char 1)
;; 			  (insert-char (+ char (- ?ア ?あ)) 1))))))

  
;; リーシﾞョン内の半角カタカナを全角カタカナに変換する
(defun miya-region-hankaku-katakana-to-zenkaku-katakana (start end)
  "リージョン内の半角カタカナを全角カタカナに変換する"
  (interactive "r")
  (miya-common-convert start end (miya-make-katakana-table) nil))


;; リージョン内の全角カタカナを半角カタカナに変換する
(defun miya-region-zenkaku-katakana-to-hankaku-katakana (start end)
  "リージョン内の全角カタカナを半角カタカナに変換する。"
  (interactive "r")
  (miya-common-convert start end (miya-make-katakana-table) t))


;; 文字列変換処理共通関数
(defun miya-common-convert (start end table find-cdr)
  (save-excursion
    (goto-char start)
    (let ((mark-active nil)
		  (end-mark (set-mark end))
		  cons)
      (while (< (point) end-mark)
	(setq cons
		  (if find-cdr
			  (assoc (char-to-string (char-after)) table)
			(rassoc (char-to-string (char-after)) table)))
	(if cons
	    (progn
	      (delete-char 1)
	      (if find-cdr
			  (insert (cdr cons))
			(insert (car cons))))
	  (forward-char 1)))
	  (set-marker end-mark nil))))


;; リージョン内の全角数字を半角数字に変換する
(defun miya-region-zenkaku-digit-to-hankaku-digit (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((mark-active nil) char)
      (while (search-forward-regexp "[０-９]" end t)
	(progn
	  (setq char (char-before))
	  (backward-delete-char 1)
	  (insert-char (- char (- ?０ ?0)) 1))))))
  

;; リージョン内の半角数字を全角数字に変換する
(defun miya-region-hankaku-digit-to-zenkaku-digit (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((mark-active nil) char)
      (while (search-forward-regexp "[0-9]" end t)
	(progn
	  (setq char (char-before))
	  (backward-delete-char 1)
	  (insert-char (+ char (- ?０ ?0)) 1))))))


;; 全角ひらがな⇔全角カタカナ変換テーブル
(defun miya-make-zenkaku-table ()
  (symbol-value
   (defvar miya-make-zenkaku-table
	 '(("あ" . "ア") ("い" . "イ") ("う" . "ウ") ("え" . "エ") ("お" . "オ")
	   ("か" . "カ") ("き" . "キ") ("く" . "ク") ("け" . "ケ") ("こ" . "コ")
	   ("さ" . "サ") ("し" . "シ") ("す" . "ス") ("せ" . "セ") ("そ" . "ソ")
	   ("た" . "タ") ("ち" . "チ") ("つ" . "ツ") ("て" . "テ") ("と" . "ト")
	   ("な" . "ナ") ("に" . "ニ") ("ぬ" . "ヌ") ("ね" . "ネ") ("の" . "ノ")
	   ("は" . "ハ") ("ひ" . "ヒ") ("ふ" . "フ") ("へ" . "ヘ") ("ほ" . "ホ")
	   ("ま" . "マ") ("み" . "ミ") ("む" . "ム") ("め" . "メ") ("も" . "モ")
	   ("や" . "ヤ") ("ゆ" . "ユ") ("よ" . "ヨ")
	   ("ら" . "ラ") ("り" . "リ") ("る" . "ル") ("れ" . "レ") ("ろ" . "ロ")
	   ("わ" . "ワ") ("を" . "ヲ") ("ん" . "ン")
	   ("が" . "ガ") ("ぎ" . "ギ") ("ぐ" . "グ") ("げ" . "ゲ") ("ご" . "ゴ")
	   ("ざ" . "ザ") ("じ" . "ジ") ("ず" . "ズ") ("ぜ" . "ゼ") ("ぞ" . "ゾ")
	   ("だ" . "ダ") ("ぢ" . "ヂ") ("づ" . "ヅ") ("で" . "デ") ("ど" . "ド")
	   ("ば" . "バ") ("び" . "ビ") ("ぶ" . "ブ") ("べ" . "ベ") ("ぼ" . "ボ")
	   ("ぱ" . "パ") ("ぴ" . "ピ") ("ぷ" . "プ") ("ぺ" . "ペ") ("ぽ" . "ポ")
	   ("ぁ" . "ァ")  ("ぃ" . "ィ")  ("ぅ" . "ゥ")  ("ぇ" . "ェ")  ("ぉ" . "ォ")
	   ("っ" . "ッ")
	   ("ゃ" . "ャ") ("ゅ" . "ュ") ("ょ" . "ョ")
	   ("ゎ" . "ワ")
	   ("う゛" . "ヴ")))))


;; 全角カタカナ⇔半角カタカナ変換テーブル
(defun miya-make-katakana-table ()
  (symbol-value
   (defvar miya-make-katakana-table
	 '(("ア" . "ｱ") ("イ" . "ｲ") ("ウ" . "ｳ") ("エ" . "ｴ") ("オ" . "ｵ")
	   ("カ" . "ｶ") ("キ" . "ｷ") ("ク" . "ｸ") ("ケ" . "ｹ") ("コ" . "ｺ")
	   ("サ" . "ｻ") ("シ" . "ｼ") ("ス" . "ｽ") ("セ" . "ｾ") ("ソ" . "ｿ")
	   ("タ" . "ﾀ") ("チ" . "ﾁ") ("ツ" . "ﾂ") ("テ" . "ﾃ") ("ト" . "ﾄ")
	   ("ナ" . "ﾅ") ("ニ" . "ﾆ") ("ヌ" . "ﾇ") ("ネ" . "ﾈ") ("ノ" . "ﾉ")
	   ("ハ" . "ﾊ") ("ヒ" . "ﾋ") ("フ" . "ﾌ") ("ヘ" . "ﾍ") ("ホ" . "ﾎ")
	   ("マ" . "ﾏ") ("ミ" . "ﾐ") ("ム" . "ﾑ") ("メ" . "ﾒ") ("モ" . "ﾓ")
	   ("ヤ" . "ﾔ") ("ユ" . "ﾕ") ("ヨ" . "ﾖ")
	   ("ラ" . "ﾗ") ("リ" . "ﾘ") ("ル" . "ﾙ") ("レ" . "ﾚ") ("ロ" . "ﾛ")
	   ("ワ" . "ﾜ") ("ヲ" . "ｦ") ("ン" . "ﾝ")
	   ("ガ" . "ｶﾞ") ("ギ" . "ｷﾞ") ("グ" . "ｸﾞ") ("ゲ" . "ｹﾞ") ("ゴ" . "ｺﾞ")
	   ("ザ" . "ｻﾞ") ("ジ" . "ｼﾞ") ("ズ" . "ｽﾞ") ("ゼ" . "ｾﾞ") ("ゾ" . "ｿﾞ")
	   ("ダ" . "ﾀﾞ") ("ヂ" . "ﾁﾞ") ("ヅ" . "ﾂﾞ") ("デ" . "ﾃﾞ") ("ド" . "ﾄﾞ")
	   ("バ" . "ﾊﾞ") ("ビ" . "ﾋﾞ") ("ブ" . "ﾌﾞ") ("ベ" . "ﾍﾞ") ("ボ" . "ﾎﾞ")
	   ("パ" . "ﾊﾟ") ("ピ" . "ﾋﾟ") ("プ" . "ﾌﾟ") ("ペ" . "ﾍﾟ") ("ポ" . "ﾎﾟ")
	   ("ァ" . "ｧ")  ("ィ" . "ｨ")  ("ゥ" . "ｩ")  ("ェ" . "ｪ")  ("ォ" . "ｫ")
	   ("ッ" . "ｯ")
	   ("ャ" . "ｬ") ("ュ" . "ｭ") ("ョ" . "ｮ")
	   ("ヮ" . "ﾜ")
	   ("ヴ" . "ｳﾞ")
	   ("ー" . "ｰ")))))


;; "リージョン内のコメント位置を調整する"(※作成中)
(defun miya-region-comment-orderline (start end)
  "リージョン内のコメント位置を調整する"
  (interactive "r")
  (let ((line-count (miya-region-count-lines start end))
		comment-text-alist
		pos
		(comment-max-column 0))
	(save-excursion
	  ;;=====================================================
	  ;; コメントのカラム位置とテキストを取得する初回ループ
	  ;;=====================================================
	  (goto-char start)
	  (do
		  ((i 1 (1+ i)))
		  ((> i line-count))
;		(setq pos (point))
;		(end-of-line)
;		(when (search-backward comment-start pos t)
		(when (search-forward comment-start (save-excursion
											  (end-of-line)
											  (point)) t)
		  (forward-char (- (length comment-start)))
		  (setq pos (point))
		  (when (< comment-max-column (current-column))
			(setq comment-max-column (current-column)))
		  (setq comment-text-alist
				(cons
				 (cons i
					   (buffer-substring (point) (progn
												   (end-of-line)
												   (point))))
				 comment-text-alist))
		  (goto-char pos)
		  (skip-chars-backward " \t　")
		  (delete-region (point) (progn (end-of-line) (point))))
		  (forward-line 1))
;	  (miya-debug-list comment-text-alist)
;	  (princ comment-max-column)
;	  (sit-for 3))
	  ;;=====================================================
	  ;; コメントを挿入する為の二回目のループ
	  ;;=====================================================
	  (goto-char start)
	  (do
		  ((i 1 (1+ i)))
		  ((> i line-count))
		(when (assq i comment-text-alist)
		  (move-to-column comment-max-column t)
		  (insert (cdr (assq i comment-text-alist))))
		(forward-line 1)))))


;; 現在の時間(秒数)を取得 <= かなりいい加減
(defun miya-get-current-sec ()
  (let ((@time (current-time)))
	(+ (second @time) (/ (float (third @time)) 1000000))))

;; 関数テスト関数
(defun miya-function-test (count func-list &optional arg-list)
  "関数テスト関数"
  (let* ((@func-count (length func-list))
		(@time-list (make-list @func-count 0))
		@func
		@ret-string)
	(dotimes
		(i @func-count)
	  (setq @func (nth i func-list))
	  (setf (nth i @time-list) (miya-get-current-sec))
	  (dotimes
		  (j count)
		(if arg-list
			(apply @func arg-list)
		  (funcall @func)))
	  (setf (nth i @time-list)
			(- (miya-get-current-sec) (nth i @time-list))))
	(dotimes
		(i @func-count)
	  (setq @func (nth i func-list))
	  (setq @ret-string
			(concat
			 (format "%s => %f\n" (symbol-name @func) (nth i @time-list))
			 @ret-string)))
	(insert "\n" @ret-string)))


;; カーソルの位置の単語をリージョンで囲む(_-も単語に含める)
;; ついでに単語をコピーする
(defun miya-mark-word ()
  (interactive)
  (let ((word "[a-zA-Z0-9_-]")
		(non-word "[^a-zA-Z0-9_-]")
		(min (save-excursion
			   (progn (beginning-of-line) (point))))
		(max (save-excursion
			   (progn (end-of-line) (point))))
		start
		end)
	(if (string-match word (char-to-string (char-after)))
		(progn
		  (if (null (search-backward-regexp non-word min t))
			  (setq start min)
			(setq start (1+ (match-beginning 0))))
		  (forward-char 1)
		  (if (null (search-forward-regexp non-word max t))
			  (setq end max)
			(setq end (match-beginning 0)))
		  (kill-new (buffer-substring start end))
		  (goto-char start)
		  (push-mark
		   (save-excursion
			 (goto-char end)
			 (point))
		   nil t))
	  (message "No word"))))

;; カーソル上の単語位置を返す
;;
;; 引数：a-z, A-Z, 0-9以外に単語と認識する文字列
;;       例)'_'を単語に含める場合 (miya-word-search "_")
;;          '_', '-'を単語に含める場合 (miya-word-search '("_" "-"))
;;
;; 戻り値：(START END)のリスト
;;         単語が見つからない場合にはnil
;; (defun miya-word-search (&optional key)
;;   (let (word-regex-basic "a-zA-Z0-9"
;; 		word-regex-extend
;; 		non-word-regex-extend

;; (defun miya-word-search-make-regex (str list)
;;   (if (null list)
;; 	  (concat "[" str "]")
;; 	(let ((key (car list)))
;; 	  (cond
;; 	   ((string= "-" key)


;; 文字列挿入
(defun miya-insert-string (str index newstr)
  (when (< (length x) index)
	(setq index (length x)))
  (let ((split (miya-split-string str index)))
	(concat (first split) newstr (second split))))

;; 文字列分割
(defun miya-split-string (str index)
	(list
	 (substring str 0 index)
	 (substring str index)))

;; リージョン内の空白行を除去する
(defun miya-delete-blank-line (start end)
  (interactive "r")
  (let ((regex "[^ 　	]")
		(blank-line-count 0)
		(marker (make-marker)))			;; (kill-line)によってリージョン終端が変化してしまうのでmarkerを作成する
	  (save-excursion
		(goto-char start)
		(set-marker marker end)
		(while (< (point) marker)
		  (if (search-forward-regexp regex
									 (save-excursion
									   (end-of-line)
									   (point))
									 t)
			  (forward-line 1)
			(when (/= (point) (point-max))
			  (kill-line)
			  (incf blank-line-count)))))
	  (message "空白行数：%d" blank-line-count)))

;; 全角スペース→半角スペース変換
(defun miya-region-zenkaku-space-to-hankaku-space (start end)
  (interactive "r")
  (miya-common-region-convert-space start end ?　 ? ))

;; 半角スペース→全角スペース変換
(defun miya-region-hankaku-space-to-zenkaku-space (start end)
  (interactive "r")
  (miya-common-region-convert-space start end ?  ?　))

;; スペース変換共通関数
(defun miya-common-region-convert-space (start end old-char new-char)
    (let ((count 0)
		(marker (make-marker)))
	(save-excursion
	  (goto-char start)
	  (set-marker marker end)
	  (while (< (point) marker)
		(if (= (char-after) old-char)
			(progn
			  (delete-char 1)
			  (insert-char new-char 1)
			  (incf count))
		  (forward-char))))
	(message "Converted %d charcter." count)))

;;複利計算
(defun miya-calc-interest (capital rate term)
  (* capital (expt (1+ rate) term)))

;;複利計算2
(defun miya-calc-interest2 (capital rate year tsuika)
  (let ((count 0))
    (setq rate (/ rate 12.0))
    (setq year (* year 12))
    (while (< count year)
      (setq capital (miya-calc-interest (+ capital tsuika) rate 1))
      (incf count))
    capital))

;; 利回り計算
(defun miya-calc-rate (before after term)
  (- (expt (/ after before) (/ 1.0 term)) 1))

;; リスト表示デバッグ用関数
(defun miya-debug-list (l &optional time)
  (unless (null l)
	(princ l)
	(princ "\n")
	(miya-debug-list (cdr l) time)))

;; 任意の範囲のlist作成
(defun make-list-of-range (from to)
  "Return the list of integers ranging from FROM to TO."
  (let (temp)
	(if (= from to)
		(cons to '())
	  (if (< from to)
		  (setq temp (1+ from))
		(setq temp (1- from)))
	  (cons from (make-list-of-range temp to)))))

;; 任意の範囲のvector作成
(defun make-vector-of-range (from to)
  "Return the vector of integers ranging from FROM to TO."
  (if (< from to)
	  (apply 'vector (make-list-of-range from to))
	(apply 'vector (reverse (make-list-of-range to from)))))

;; x°毎の時間からNEを算出(単位us)
(defun calNE (time)
  (interactive "n時間(us): ")
  (let ((angle (read-number "角度: " 30)))
	(message "%d[rpm]"
			 (/ 1 (/ (* time (/ 360 (float angle))) 60000000)))))

;; NEからx°毎の時間を算出(単位us)
(defun calME (ne)
  (interactive "n回転数(rpm): ")
  (let ((angle (read-number "角度: " 30)))
	(message "%d[us]"
			 (/ (/ 60000000.0 ne) (/ 360 (float angle))))))

;; 16進数表示
(defmacro hex-print (param) (list 'format "#x%X" param))

;; カレントウィンドウの透明度設定
(when (>= emacs-major-version 22)
  (defun miya-set-frame-alpha (num)
	(interactive "n透明度(20 - 100):")
	(let (list)
	  (setq list (make-list 2 num))
	  (set-frame-parameter nil 'alpha list))))

;; 'global -u'コマンド実行
(defun miya-remake-gtags ()
  (interactive)
  (let* ((root-dir (if (require 'helm-gtags nil t)
					   (helm-gtags--tag-directory)
					 (gtags-get-rootpath)))
		 (cur-dir default-directory))
	(if root-dir
		(progn
		  (cd-absolute root-dir)
		  (call-process "global" nil nil nil "-u")
		  (cd-absolute cur-dir)
		  (message "GTAGS remade.")
		  )
	  (message "GTAGS not found."))))

;; ディレクトリ内の"*.el"ファイルをバイトコンパイル
(defun miya-byte-compile-folder (folder)
  (interactive "D")
  (let ((files (directory-files folder t "\.el$")))
	(while files
	  (when (not (byte-compile-file (car files)))
		(error (format "%sのコンパイルに失敗しました。" (car files))))
	  (setq files (cdr files)))))

;; ポイント位置の行数を返す
;; ※ current-columnは標準であるが、なぜか
;;    行数はないので"moccur.el"から拝借
;(defun current-line ()
;  "Return the vertical position of point..."
;  (1+ (count-lines 1 (point))))

;;=========================================================
;; バッファとウインドウを削除
;;=========================================================
(defun kill-buffer-and-window ()
  (interactive)
  (if (kill-buffer (current-buffer))
	  (delete-window)))
