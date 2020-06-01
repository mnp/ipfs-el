;; -*- Lisp-Interaction -*-
;; Requires internet connection and local IPFS node running on 5001 and 8080

(require 'elt)
(require 'ipfs)

(defconst hello-cid "QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")
(defconst hello-exp "hello\n")
(defconst pants-cid "QmXGVLoWoyvP4NYwDwU231oDufMBschvAz99bbh9EyxrrL")
(defconst pants-exp "pants\n")

(defun quickie-uuid (bytes)
  (let ((str ""))
    (dotimes (i bytes)
      (setq str (concat str (format "%0x" (random 256)))))
    str))

(defun should-contain (regex str)
  (should (string-match-p regex str)))

(ert-deftest hello-local ()
  (let ((got (ipfs-cat-synchronous hello-cid)))
    (should (equal got hello-exp))))

(ert-deftest hello-gateway ()
  (let* ((ipfs-node-url ipfs-public-gateway-url)
         (got (ipfs-cat-synchronous hello-cid)))
    (should (equal got hello-exp))))

(ert-deftest version-local ()
  (should-contain "Version" (ipfs-version-synchronous)))

(ert-deftest version-gateway ()
  (let* ((ipfs-node-url ipfs-public-gateway-url))
    (should-contain "Version" (ipfs-version-synchronous))))

(ert-deftest add-local-preset ()
  (should (equal (ipfs-add-synchronous "pants\n") '(File . 4)))
  (should (equal (ipfs-cat-synchronous pants-cid) pants-exp)))

;; Random data means it's new, ie not in my node or in the network anywhere.
(ert-deftest add-local-random-data ()
  (let* ((data (quickie-uuid 10))
         (resp (ipfs-add-synchronous data)))
    ;; TODO
    t))

(ert-deftest upstream-fetch ()
  (let ((archive (ipfs-fetch-upstream)))
    (should (equal 1 (car archive)))
    (should (< 4000 (length (cdr archive))))))

;; TODO
;(should-error 
; (url-retrieve-synchronously "badurl")
