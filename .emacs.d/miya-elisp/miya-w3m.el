;;;; -*- coding: utf-8 -*-

;;-----------------------------------------------------------------------------
;; w3m設定ファイル
;;-----------------------------------------------------------------------------

(autoload 'w3m "w3m"
  "Visit the WWW page using w3m" t)

(autoload 'w3m-find-file "w3m"
  "Find a local file using emacs-w3m." t)
(autoload 'w3m-browse-url "w3m"
  "Ask emacs-w3m to show a URL." t)
(autoload 'w3m-antenna "w3m-antenna"
  "Report changes of web sites." t)
(autoload 'w3m-bookmark-view "w3m-bookmark"
  "Show bookmarks." t)
(autoload 'w3m-dtree "w3m-dtree"
  "Display a directory tree." t)
(autoload 'w3m-namazu "w3m-namazu"
  "Search files with Namazu." t)
(autoload 'w3m-perldoc "w3m-perldoc"
  "View Perl documents" t)
(autoload 'w3m-search "w3m-search"
  "Search words using emacs-w3m." t)
(autoload 'w3m-weather "w3m-weather"
  "Display a weather report." t)


;;w3mコマンドのデフォルト表示ページ
(setq w3m-home-page "http://www.yahoo.co.jp")

;;w3m起動時にＵＲＬを指定する
;(setq w3m-quick-start nil)

;;デフォルトの画像表示モード設定
(setq w3m-default-display-inline-images t)

;;履歴保存数(デフォルト500)
;(setq w3m-keep-arrived-urls 500)

;; プロキシ設定
(setq w3m-command-arguments
                (nconc w3m-command-arguments
                       '("-o" "http_proxy=http://proxy.han.shindengen.co.jp:8080/")))

;; クッキー設定
(setq w3m-use-cookies t)

;; プロキシを使わないURL
(setq w3m-no-proxy-domains '("127.0.0.1" "localhost"))
