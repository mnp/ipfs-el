(require 'package)
;(setq debug-on-error t)

(add-to-list 'load-path default-directory)

;; add melpa stable
(add-to-list 'package-archives
         '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; add melpa
(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
(package-refresh-contents)
(package-install 'http-post-simple)

(load-file "ipfs-test.el")
(load-file "ipfs-integ-test.el")

(ert-run-tests-batch-and-exit)
