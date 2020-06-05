(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(package-install 'http-post-simple)
(require 'http-post-simple)

(require 'ert)
(require 'ipfs)

(ert t)
