;; -*- Lisp-Interaction -*-

;; A few experiments interfacing with the IPFS daemon HTTP protocol.
;; https://docs.ipfs.io/guides/concepts/dht/

;; TODO: look into supporting an ipfs://  scheme
;; TODO: how does package load non-URLs
;; TODO: package.el archive hooks
;; TODO: Augment or reformat?: melpa.org/packages/archive-contents file, created by melpa repo Makefile

(require 'url-http)   ; ?
(require 'json)

;; Addressing and what it means documented here
;; https://github.com/ipfs/in-web-browsers/blob/master/ADDRESSING.md

(defvar ipfs-upstream-url "https://melpa.org/packages/archive-contents")
(defvar ipfs-cid-protocol "ipfs://")
(defvar ipfs-ipns-protocol "ipns://")

(defvar ipfs-daemon-url "http://127.0.0.1:5001/"
  "The daemon usually runs locally.")

(defconst ipfs-boundary "---2a8ae6ad-f4ad-4d9a-a92c-6d217011fe0f---")

(defun ipfs-fetch-upstream ()
  (with-current-buffer
      (url-https
       (url-generic-parse-url ipfs-upstream-url)
       (lambda (x)
         (goto-char (point-min))
         (re-search-forward "\n\n")
         (let ((tmp (read (current-buffer))))
           (if (eq 1 (car tmp))
               (progn 
                 (message "Retrieved %s packages from %s" (length tmp) ipfs-upstream-url)
                 (setq ipfs-archive-contents tmp))
             (error "Bad read from %s" ipfs-upstream-url))))
       nil))
  t)
(ipfs-fetch-upstream)


; ipfs-archive-contents 



; (mapc 'kill-buffer (seq-filter (lambda (x) (string-match-p "melpa" (buffer-name x)))
;                                (buffer-list)))


;; works 
;; (ipfs-fetch-upstream)
;; (mapcar 'car ipfs-archive-contents)



; MAYBE
; (custom-set-variables '(url-handler-regexp
;                        "\\`\\(https?\\|ipfs\\|ipns\\|ftp\\|file\\|nfs\\|ssh\\|scp\\|rsync\\|telnet\\)://"))

; (url-handler-mode 1)
; (find-file "https://example.com")

;; file-name-handler-alist
;; (("\\`\\(https?\\|ipfs\\|ipns\\|ftp\\|file\\|nfs\\|ssh\\|scp\\|rsync\\|telnet\\)://" . url-file-handler) ("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler) ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler) ("\\`/\\(\\([^/|:]+:[^/|:]*|\\)*[^/|:]+\\(:[^/|:]*\\)?\\)?\\'" . tramp-completion-file-name-handler) ("^/\\(\\(?:\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?|\\)+\\)?\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?:\\(.*$\\)" . tramp-file-name-handler) ("\\`/:" . file-name-non-special))


;; (url-generic-parse-url "ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html")
;; #s(url "ipfs" nil nil "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq" nil "/wiki/Vincent_van_Gogh.html" nil nil t nil t t)
;; 
;; (url-generic-parse-url  "http://127.0.0.1:5001/")
;; #s(url "http" nil nil "127.0.0.1" 5001 "/" nil nil t nil t t)

url-file-handlers


(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

;"http://127.0.0.1:5001/api/v0/version"
;"http://127.0.0.1:5001/api/v0/swarm/disconnect?arg=/ip4/54.93.113.247/tcp/48131/ipfs/QmUDS3nsBD1X4XK5Jo836fed7SErTyTuQzRqWaiQAyBYMP"))

(defun ipfs-get-api (path)
  (ipfs-get-url (concat ipfs-daemon-url path)))

(defun ipfs-get-url (url)
  (let ((url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously url)
 (cadr (split-string (buffer-string) "\n\n")))))


;      (json-read-from-string

(defun ipfs-add (body)
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . (concat "Content-Type: multipart/form-data; boundary=" ipfs-boundary))))
         (url (concat ipfs-daemon-url "api/v0/add")))
    (with-current-buffer (url-retrieve-synchronously url)
      (read-from-string (cadr (split-string (buffer-string) "\n\n"))))))

; ok (ipfs-get-api "/api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")
http://example.com

; ok(ipfs-get-api "/api/v0/cat?arg=QmVeAzDB73nzB8aYrTtkrqQfPKxEr56jPfTVB21UsdNAMp")

(defun ipfs-add-string (str)
  (http-post-simple-multipart   
   (concat ipfs-daemon-url "api/v0/add")
   nil
   (list (list "path" "ipfs.el" "text/plain" str))))


;; works
;;
 (ipfs-add-string "bleh")

;; works
;; (byte-compile 'ipfs-get-url)

;; Try out http-post-simple
;
; ok
;
; (http-post-simple-multipart 
;  (concat ipfs-daemon-url "api/v0/add")
;  nil
;  '(("path" "ipfs.el" "text/plain" "hello this is the file data\nit is very nice")))
; ("{\"Name\":\"ipfs.el\",\"Hash\":\"QmVeAzDB73nzB8aYrTtkrqQfPKxEr56jPfTVB21UsdNAMp\",\"Size\":\"51\"}
; " "HTTP/1.1 200 OK
; Access-Control-Allow-Headers: X-Stream-Output, X-Chunked-Output, X-Content-Length
; Access-Control-Expose-Headers: X-Stream-Output, X-Chunked-Output, X-Content-Length
; Content-Type: application/json
; Server: go-ipfs/0.5.1
; Trailer: X-Stream-Error
; Vary: Origin
; X-Chunked-Output: 1
; Date: Mon, 18 May 2020 13:24:30 GMT
; Transfer-Encoding: chunked
; " 200)








(http-post-encode-multipart-data nil q
