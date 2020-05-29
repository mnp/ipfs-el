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
  "Retreive upstream archive-contents and return it as a list."
  (with-current-buffer
    (url-retrieve-synchronously ipfs-upstream-url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (let ((fetched (read (current-buffer))))
      (kill-buffer (current-buffer))
      (if (and fetched (listp fetched) (eq 1 (car fetched)))
          (progn 
            (message "Retrieved %s packages from %s" (length (cdr fetched)) ipfs-upstream-url)
            fetched)
        (error "Bad read from %s" ipfs-upstream-url)))))

;; works 
;; (mapcar 'car (car (ipfs-fetch-upstream)))

; ---- NO ----
; (custom-set-variables '(url-handler-regexp
;                        "\\`\\(https?\\|ipfs\\|ipns\\|ftp\\|file\\|nfs\\|ssh\\|scp\\|rsync\\|telnet\\)://"))

; ---- YES ----
; (url-handler-mode 1)
; (find-file "https://example.com")

;; file-name-handler-alist
;; (("\\`\\(https?\\|ipfs\\|ipns\\|ftp\\|file\\|nfs\\|ssh\\|scp\\|rsync\\|telnet\\)://" . url-file-handler) ("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler) ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler) ("\\`/\\(\\([^/|:]+:[^/|:]*|\\)*[^/|:]+\\(:[^/|:]*\\)?\\)?\\'" . tramp-completion-file-name-handler) ("^/\\(\\(?:\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?|\\)+\\)?\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?:\\(.*$\\)" . tramp-file-name-handler) ("\\`/:" . file-name-non-special))


;; (url-generic-parse-url "ipfs://bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq/wiki/Vincent_van_Gogh.html")
;; #s(url "ipfs" nil nil "bafybeiemxf5abjwjbikoz4mc3a3dla6ual3jsgpdr4cjr3oz3evfyavhwq" nil "/wiki/Vincent_van_Gogh.html" nil nil t nil t t)
;; 
;; (url-generic-parse-url  "http://127.0.0.1:5001/")
;; #s(url "http" nil nil "127.0.0.1" 5001 "/" nil nil t nil t t)

(defconst ipfs-handler-regexp "\\(ipfs\\|ipns\\)://"
  "IPFS and IPNS protocol URL regexp")

(defun ipfs-file-handler (operation &rest args)
    "Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done (`file-exists-p', etc).  ARGS are
the arguments that would have been passed to OPERATION."
    (message "ipfs-file-handler got (%s %s)" operation args)
    (cond ((eq operation 'insert-file-contents) (error "XX"))
                       ;; Handle any operation we don't know about.
          (t (let ((inhibit-file-name-handlers
                    (cons 'ipfs-file-handler
                          (and (eq inhibit-file-name-operation operation)
                               inhibit-file-name-handlers)))
                   (inhibit-file-name-operation operation))
               (apply operation args)))))
ipfs-file-handler



(url-handler-mode 1)
t

(find-file "ipfs://a/b")

(file-exists-p "ipfs://a/b")

(file-attributes "ipfs://foo")
     (funcall #'ipfs-file-handler 'file-exists-p "xxx")
 
(access-file "ipfs://xxtmp" "x")


package-archives


(setq file-name-handler-alist
'(("\\`\\(https?\\|ftp\\|file\\|nfs\\|ssh\\|scp\\|rsync\\|telnet\\)://" . url-file-handler) ("\\`ipfs://" . ipfs-file-handler) ("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler) ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" . epa-file-handler) ("\\`/\\(\\([^/|:]+:[^/|:]*|\\)*[^/|:]+\\(:[^/|:]*\\)?\\)?\\'" . tramp-completion-file-name-handler) ("^/\\(\\(?:\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?|\\)+\\)?\\([a-zA-Z0-9-]+\\):\\(?:\\([^/|: 	]+\\)@\\)?\\(\\(?:[a-zA-Z0-9_.%-]+\\|\\[\\(?:\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+\\)?]\\)\\(?:#[0-9]+\\)?\\)?:\\(.*$\\)" . tramp-file-name-handler) ("\\`/:" . file-name-non-special)))


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

; ok  (ipfs-get-api "/api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN"))

(unwind-protect
    (url-retrieve "http://127.0.0.1:5088/")
    (url-retrieve "http://google.com" (lambda (status ) (message "callback %s" status)))
#<buffer  *http google.com:80*-487123>

(catch 'x (url-retrieve-synchronously "foo"))
(unwind-protect (error "foo") (setq xx "oopsie"))
(condition-case 666
    (error nil)

(setq baz 3)

     (condition-case err
         (error "Rats!")
       ;; This is the handler; it is not a form.
       (error (princ (format "The error was: %s" err))
              2))
The error was: (error Rats!)2





(set-variable debug-on-error nil)

Debugger entered--Lisp error: (file-error "make client process failed" "Connection refused" :name "127.0.0.1" :buffer #<buffer  *url-http-temp*> :host "127.0.0.1" :service 5088 :nowait nil :tls-parameters nil)
  make-network-process(:name "127.0.0.1" :buffer #<buffer  *url-http-temp*> :host "127.0.0.1" :service 5088 :nowait nil :tls-parameters nil)
  open-network-stream("127.0.0.1" #<buffer  *url-http-temp*> "127.0.0.1" 5088 :type plain :nowait nil)
  url-open-stream("127.0.0.1" #<buffer  *url-http-temp*> "127.0.0.1" 5088 nil)
  url-http-find-free-connection("127.0.0.1" 5088 nil)
  url-http(#s(url :type "http" :user nil :password nil :host "127.0.0.1" :portspec 5088 :filename "//api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN" :target nil :attributes nil :fullness t :silent nil :use-cookies t :asynchronous nil) #f(compiled-function (&rest ignored) #<bytecode 0x304572d>) (nil))
  url-retrieve-internal("http://127.0.0.1:5088//api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN" #f(compiled-function (&rest ignored) #<bytecode 0x304572d>) (nil) nil nil)
  url-retrieve("http://127.0.0.1:5088//api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN" #f(compiled-function (&rest ignored) #<bytecode 0x304572d>) nil nil nil)
  url-retrieve-synchronously("http://127.0.0.1:5088//api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")
  (set-buffer (url-retrieve-synchronously url))
  (save-current-buffer (set-buffer (url-retrieve-synchronously url)) (car (cdr (split-string (buffer-string) "\n\n"))))
  (let ((url-request-method "POST")) (save-current-buffer (set-buffer (url-retrieve-synchronously url)) (car (cdr (split-string (buffer-string) "\n\n")))))
  ipfs-get-url("http://127.0.0.1:5088//api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")
  ipfs-get-api("/api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")
  (let ((ipfs-daemon-url "http://127.0.0.1:5088/")) (ipfs-get-api "/api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN"))
  eval((let ((ipfs-daemon-url "http://127.0.0.1:5088/")) (ipfs-get-api "/api/v0/cat?arg=QmZULkCELmmk5XNfCgTnCyFgAVxBRBXyDHGGMVoLFLiXEN")) nil)



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
