;; -*- Lisp-Interaction -*-

;; A few experiments interfacing with the IPFS daemon HTTP protocol.
;; https://docs.ipfs.io/guides/concepts/dht/

;; TODO: look into supporting an ipfs://  scheme
;; TODO: how does package load non-URLs
;; TODO: package.el archive hooks
;; TODO: Augment or reformat?: melpa.org/packages/archive-contents file, created by melpa repo Makefile

(require 'http-post-simple)
(require 'url-http)
(require 'json)

(defvar ipfs-node-url "http://127.0.0.1:5001"
  "The daemon usually runs locally.")

(defvar ipfs-public-gateway-url "https://ipfs.io"
  "Public IPFS gateway used if `ipfs-node-url` is not reachable. Read only.")

(defvar ipfs-cid-protocol "ipfs://")

(defvar ipfs-ipns-protocol "ipns://")

(defun ipfs-api-synchronous (command fields files)
  "Performs a synchronous POST and returns a list of response (either JSON or text like from a `/cat`),
  headers, and HTTP result code."
  (http-post-simple-multipart   
   (concat ipfs-node-url "/api/v0/" command)
   fields
   files))

(defun ipfs-api-json (command fields files)
  (json-read-from-string (car (ipfs-api-synchronous command fields files))))

(defun ipfs-api-text (command fields files)
  (car (ipfs-api-synchronous command fields files)))


;;; API Commands. Defuns for now but they could become defmacros.

(defun ipfs-cat (cid)
  "Returns string if object at CID is a file."
  (ipfs-api-text "cat" `((arg . ,cid)) nil))

(defun ipfs-version ()
  "Returns list alis of Version, Commit, Repo, System, Golang"
  (ipfs-api-json "version" nil nil))

(defun ipfs-add-string (str)
  "Returns hash of newly added string."
  (ipfs-api-json "add" `((path . str)) nil))

(defun ipfs-stat (cid)
  "Examines object at CID and returns alist with keys `Hash`, `NumLinks`, `BlockSize`, `LinkSize`, `DataSize`, and `CumulativeSize`."
  (ipfs-api-json "object/stat" `((arg . ,cid)) nil))

;; ... etc TODO ...

;(unwind-protect
;    (url-retrieve "http://127.0.0.1:5088/")
;    (url-retrieve "http://google.com" (lambda (status ) (message "callback %s" status)))
;
; ok(ipfs-get-api "/api/v0/cat?arg=QmVeAzDB73nzB8aYrTtkrqQfPKxEr56jPfTVB21UsdNAMp")

; ;; file
; (ipfs-stat "QmVeAzDB73nzB8aYrTtkrqQfPKxEr56jPfTVB21UsdNAMp")
; ((Hash . "QmVeAzDB73nzB8aYrTtkrqQfPKxEr56jPfTVB21UsdNAMp") (NumLinks . 0) (BlockSize . 51) (LinksSize . 2) (DataSize . 49) (CumulativeSize . 51))
; 
; ;; directory 
; (ipfs-stat "QmZAyK3gEk9guF2zPsr6S9X7PJ8Y56XvTeqtzKL9h3HpdN")
; ((Hash . "QmZAyK3gEk9guF2zPsr6S9X7PJ8Y56XvTeqtzKL9h3HpdN") (NumLinks . 11) (BlockSize . 651) (LinksSize . 649) (DataSize . 2) (CumulativeSize . 1580429))


;; TODO FIXME
;; (ipfs-api-text "files/stat" '((path . "QmUPLMqRWm5u6GgfWb777wb9qLDc9RTQf5rqy1ymStMbaP")) nil)
;; "argument \"path\" is required


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



;;; File Handler and Mode

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

(define-minor-mode ipfs-mode
  "Toggle enabling of IPFS minor mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state.

     When IFPS mode is enabled, file handlers are added for paths beginning with `/ipfs` and
     `/ipns`. Disabling it removes the special handlers."
  nil	      ;; The initial value.
  "ipfs"      ;; The indicator for the mode line.
  nil	      ;; The minor mode bindings.
  :group 'ipfs
  :after-hook (if dispwatch-mode
		  (ipfs-enable)
		  (ipfs-disable)))

(defconst ipfs-file-handler-regexp "\\`/\\(ipfs\\|ipns\\)"
  "IPFS and IPNS protocol path regexp")

(defconst ipfs-url-handler-regexp "\\(ipfs\\|ipns\\)://"
  "IPFS and IPNS protocol URL regexp")

(defun ipfs-file-handler (operation &rest args)
    "Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done (`file-exists-p', etc).  ARGS are
the arguments that would have been passed to OPERATION."
    (message "ipfs-file-handler working on (%s %s)" operation args)

    (let ((path (car args)))
      (cond 
          ;; TODO: &optional VISIT BEG END REPLACE
          ((eq operation 'insert-file-contents) (ipfs-cat path))

          (t (let ((inhibit-file-name-handlers
                    (cons 'ipfs-file-handler
                          (and (eq inhibit-file-name-operation operation)
                               inhibit-file-name-handlers)))
                   (inhibit-file-name-operation operation))
               (apply operation args))))))

;; works
;; (insert-file-contents "/ipfs/QmVeAzDB73nzB8aYrTtkrqQfPKxEr56jPfTVB21UsdNAMp")


(defun ipfs-enable ()
  "Install IPFS magic file handlers."
  (add-to-list 'file-name-handler-alist (cons ipfs-file-handler-regexp #'ipfs-file-handler)))

(defun ipfs-disable ()
  "Remove IPFS magic file handlers."
  (delete (rassoc #'ipfs-file-handler file-name-handler-alist) file-name-handler-alist))


;;; Package Manager
 
(defvar ipfs-upstream-url "https://melpa.org/packages/archive-contents"
  "Location of upstream package index.")

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
                                 
(provide 'ipfs)
