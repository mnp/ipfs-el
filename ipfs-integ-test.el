;; -*- Lisp-Interaction -*-
;; Requires internet connection and local IPFS node running on 5001 and 8080

;; TODO: makefile normalize: https://nullprogram.com/blog/2020/01/22/

(require 'ert)
(require 'ipfs)

(defconst hello-cid "QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")
(defconst hello-exp "hello\n")
(defconst pants-cid "QmVuzYfM6avHDorxDfLJWxcsP6XdqdDk3RQzsUHNbtR4Fo")
(defconst pants-exp "pants")

(defun quickie-uuid (bytes)
  (let ((str ""))
    (dotimes (i bytes)
      (setq str (concat str (format "%0x" (random 256)))))
    str))

(defun should-contain (regex str)
  (should (string-match-p regex str)))

(ert-deftest hello-local ()
  (let ((got (ipfs-cat hello-cid)))
    (should (equal got hello-exp))))

(ert-deftest hello-gateway ()
  (let* ((ipfs-node-url ipfs-public-gateway-url)
         (got (ipfs-cat hello-cid)))
    (should (equal got hello-exp))))

(ert-deftest version-local ()
  (let ((ver-alist (ipfs-version)))
    (dolist (key '(Version Commit Repo System Golang))
      (should (assoc key ver-alist)))))

(ert-deftest version-gateway ()
  (let* ((ipfs-node-url ipfs-public-gateway-url)
         (ver-alist (ipfs-version)))
    (dolist (key '(Version Commit Repo System Golang))
      (should (assoc key ver-alist)))))

(ert-deftest add-local-preset ()
  (should (equal (ipfs-add-string pants-exp) pants-cid))
  (should (equal (ipfs-cat pants-cid) pants-exp)))

;; Random data means it's new, ie not in my node or in the network anywhere.
(ert-deftest add-local-random-data ()
  (let* ((data (quickie-uuid 10))
         (newcid (ipfs-add-string data))
         (retrv (ipfs-cat newcid)))
    (should (< 44 (length newcid)))
    (should (equal retrv data))))       ;; good readback

(ert-deftest upstream-fetch ()
  (let ((archive (ipfs-fetch-upstream)))
    (should (equal 1 (car archive)))
    (should (< 4000 (length (cdr archive))))))


;; TODO
;(should-error 
; (url-retrieve-synchronously "badurl")
