;; Install some dependencies

(add-to-list 'load-path default-directory)
(setq package-user-dir default-directory)

(require 'cl)
(require 'package)

;; add melpa stable
(add-to-list 'package-archives
         '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; add melpa
(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
(package-refresh-contents)
(package-install 'http-post-simple)

