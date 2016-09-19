;; Customize 用のグループを追加．
(defgroup Meadow-Memo nil
  "Meadow Memo が配布するパッケージ関連の設定"
  :group 'emacs)

(defcustom YAMA-binary-files-editor t
  "nil 以外であれば，バイナリファイルを開く際に
バイナリエディタとして編集するかどうかを選択できる"
  :type 'boolean
  :group 'Meadow-Memo)

(defcustom YAMA-binary-use-xdoc2txt
  (if
      (and
       (or
        (locate-library
         shell-file-name nil exec-path)
        (locate-library
         (concat shell-file-name ".exe")
         nil exec-path))
       (locate-library
        "xdoc2txt.exe" nil exec-path))
      t
    nil)
  "nil 以外であれば，doc などの拡張子でバイナリ
ファイルの場合には，xdoc2txt を使って開くようにする．"
  :type 'boolean
  :group 'Meadow-Memo)

(defcustom YAMA-file-not-binary-extensions '()
  "バイナリとみなさないファイルの拡張子を指定する．
  ただし，すべて小文字で指定する"
  :type '(repeat string)
  :group 'Meadow-Memo)

(defcustom YAMA-file-not-binary-files
  '("tags" "gsyms" "gpath" "grtags" "gsyms" "gtags")
  "バイナリとみなさないファイル名を指定する．
ただし，すべて小文字で指定のこと"
  :type '(repeat string)
  :group 'Meadow-Memo)

(defcustom YAMA-binary-xdoc2txt-exts
  '(
    "\\.rtf" "\\.doc" "\\.xls" "\\.ppt"
    "\\.jaw" "\\.jtw" "\\.jbw" "\\.juw"
    "\\.jfw" "\\.jvw" "\\.jtd" "\\.jtt"
    "\\.oas" "\\.oa2" "\\.oa3" "\\.bun"
    "\\.wj2" "\\.wj3" "\\.wk3" "\\.wk4"
    "\\.123" "\\.wri" "\\.pdf" "\\.mht")
  "*List of file extensions which are handled by xdoc2txt.
ただし，すべて小文字で指定のこと"
  :type '(repeat string)
  :group 'Meadow-Memo)

(defun Yama-file-correspond-ext-p (filename list)
  (let ((ret nil))
    (while list
      (when (string-match (car list) filename)
        (setq ret t))
      (setq list (cdr list)))
    ret))

(defun YAMA-file-binary-p (file &optional full)
  "Return t if FILE contains binary data.  If optional FULL
 is non-nil, check for the whole contents of FILE, otherwise
 check for the first 1000-byte."
  (let ((coding-system-for-read 'binary)
        default-enable-multibyte-characters)
    (if (or
         (not YAMA-binary-files-editor)
         (and
          (boundp 'image-types)
          (not (Yama-file-correspond-ext-p
                file YAMA-binary-xdoc2txt-exts))
          (or
           (memq (intern (upcase (file-name-extension file)))
                 image-types)
           (memq (intern (downcase
                          (file-name-extension file)))
                 image-types)))
         (member (downcase (file-name-extension file))
                 YAMA-file-not-binary-extensions)
         (member (downcase (file-name-nondirectory file))
                 YAMA-file-not-binary-files))
        nil
      (with-temp-buffer
        (insert-file-contents file nil 0
                              (if full nil 2000))
        (goto-char (point-min))
        (cond
         ((re-search-forward
           "[\000-\010\016-\032\034-\037]"
           nil t)
          (if (and YAMA-binary-use-xdoc2txt
                   (Yama-file-correspond-ext-p
                    file YAMA-binary-xdoc2txt-exts))
              1
            0))
         (t nil))))))

(defvar mmemo-buffer-file-name nil)
(make-variable-buffer-local
 'mmemo-buffer-file-name)

(defun Yama-binary-file-view (file)
  (let ((dummy-buff
         (generate-new-buffer
          (concat "xdoc2txt:"
                  (file-name-nondirectory
                   file))))
        (coding-system-for-write 'binary)
        (coding-system-for-read 'binary))
    (set-buffer dummy-buff)
    (make-variable-buffer-local
     'mmemo-buffer-file-name)
    (setq mmemo-buffer-file-name file)
    (let ((fn (concat
               (expand-file-name
                (make-temp-name "xdoc2")
                temporary-file-directory)
               "."
               (file-name-extension file)))
          (str nil)
          )
      (set-buffer-file-coding-system 'euc-japan)

      (copy-file file fn t)
      (insert
       "XDOC2TXT FILE: " (file-name-nondirectory file) "\n"
       "----------------------------------------------------\n"
       (shell-command-to-string
        (concat
         "cd " (file-name-directory fn) ";"
         "xdoc2txt" " -e " (file-name-nondirectory fn))))
      (goto-char (point-min))
      (end-of-line)
      (decode-coding-region (point) (point-max)
                            'euc-jp)
      (while (re-search-forward "\r" nil t)
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward
              "\\([\n ]+\\)\n[ ]*\n" nil t)
        (delete-region (match-beginning 1)
                       (match-end 1)))
      (delete-file fn)
      )
    (setq buffer-read-only t)
    (set-window-buffer (selected-window) dummy-buff))
  (goto-char (point-min))
  (view-mode t))

(defadvice find-file
  (around YAMA-find-file (file &optional wild))
  (let ((bn (condition-case nil
                (YAMA-file-binary-p file) (error nil))))
    (cond
     ((and
       (not coding-system-for-read)
       (eq bn 1)
       (y-or-n-p
        "バイナリデータの内容を xdoc2txt で表示しますか?"))
      (Yama-binary-file-view file))
     ((and
       (not coding-system-for-read)
       (eq bn 0)
       (y-or-n-p "バイナリデータとして編集しますか?"))
      (hexl-find-file file))
     (t
      ad-do-it))))

(ad-activate 'find-file)
