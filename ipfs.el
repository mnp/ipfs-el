;; -*- Lisp-Interaction -*-

;; A few experiments interfacing with the IPFS daemon HTTP protocol.
;; https://docs.ipfs.io/guides/concepts/dht/

(require 'url-http)   ; ?
(require 'json)

(defvar ipfs-daemon-url "http://127.0.0.1:5001/"
  "The daemon usually runs locally.")

(defconst ipfs-boundary "---2a8ae6ad-f4ad-4d9a-a92c-6d217011fe0f---")

; https://www.emacswiki.org/emacs/UrlPackage

(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

;"http://127.0.0.1:5001/api/v0/version"
;"http://127.0.0.1:5001/api/v0/swarm/disconnect?arg=/ip4/54.93.113.247/tcp/48131/ipfs/QmUDS3nsBD1X4XK5Jo836fed7SErTyTuQzRqWaiQAyBYMP"))

(defun ipfs-get-api (path)
  (ipfs-get-url (concat ipfs-daemon-url path)))

(defun ipfs-get-json (url)
  (let ((url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously url)
      (json-read-from-string (cadr (split-string (buffer-string) "\n\n"))))))

(defun ipfs-add (body)
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . (concat "Content-Type: multipart/form-data; boundary=" ipfs-boundary))))
         (url (concat ipfs-daemon-url "api/v0/add")))
    (with-current-buffer (url-retrieve-synchronously url)
      (read-from-string (cadr (split-string (buffer-string) "\n\n"))))))

(ipfs-get-api "/api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")


; works
; (byte-compile 'ipfs-get-url)


;; Try out http-post-simple

(http-post-simple-multipart 
 (concat ipfs-daemon-url "api/v0/add") 
 nil 
 '("foo" "ipfs.el" "text/plain" "hello this is the file data\nit is very nice"))

