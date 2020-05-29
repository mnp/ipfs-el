(require 'ipfs)

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))


;; ---- Asynchronous -----

(defun retrieve-handler (status)
  (message "callback %s with %s chars" status (buffer-size))
  (setq got status))

(condition-case x (url-retrieve "http://ahsdjfas" #'retrieve-handler) (t (message "handle %s" x)))
"handle (error ahsdjfas/80 nodename nor servname provided, or not known)"

(condition-case x (url-retrieve "http:ahsdjfas" #'retrieve-handler) (t (message "handle %s" x)))
"handle (wrong-type-argument stringp nil)"

(condition-case x (url-retrieve "foo" #'retrieve-handler) (t (message "handle %s" x)))
"handle (error Bad url: foo)"

(condition-case x (url-retrieve "http://localhost:2222" #'retrieve-handler) (t (message "handle %s" x)))
#<buffer  *http localhost:2222*-616811>
got
(:error (error connection-failed "failed with code 61
" :host "localhost" :service 2222))

(condition-case x (url-retrieve "http://example.com" #'retrieve-handler) (t (message "handle %s" x)))
#<buffer  *http example.com:80*>
got
nil

--> callback nil with 1598 chars



;; -------------- Synchronous -----------

(condition-case x (url-retrieve-synchronously "foo") (t (message "handle %s" x)))
"handle (error Bad url: foo)"

(condition-case x (url-retrieve-synchronously "http:asdfasfa") (t (message "handle %s" x)))
"my handle (wrong-type-argument stringp nil)"

(condition-case x (url-retrieve-synchronously "http://asdfasdfasdf") (t (message "handle %s" x)))
"handle (error asdfasdfasdf/80 nodename nor servname provided, or not known)"

(condition-case x (url-retrieve-synchronously "http://4.44.55.66") (t (message "handle %s" x)))
"handle (quit)"

(condition-case x (url-retrieve-synchronously "http://localhost:6666") (t (message "handle %s" x)))
"handle (file-error make client process failed Connection refused :name localhost :buffer #<killed buffer> :host localhost :service 6666 :nowait nil :tls-parameters nil)"

(condition-case x (url-retrieve-synchronously "https://example.com") (t (message "handle %s" x)))
#<buffer  *http example.com:443*>

