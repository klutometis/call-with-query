@(egg "call-with-query")
@(description "A reasonable abstraction around e.g. fastcgi server-invocations: ports, environment, query")
@(author "Peter Danenberg")
@(username "klutometis")
@(email "pcd@roxygen.org")
@(noop)

(define-record-and-printer query
  @("Data structure to hold the query."
    (server "Server variables, e.g. environment")
    (client "Client variables, e.g. {get,post,cookie}-parameters"))
  server client)

(define status-continue 100)
(define status-switching-protocols 101)
(define status-ok 200)
(define status-created 201)
(define status-accepted 202)
(define status-non-authoritative-information 203)
(define status-no-content 204)
(define status-reset-content 205)
(define status-partial-content 206)
(define status-multiple-choices 300)
(define status-moved-permanently 301)
(define status-found 302)
(define status-see-other 303)
(define status-not-modified 304)
(define status-use-proxy 305)
(define status-unused 306)
(define status-temporary-redirect 307)
(define status-bad-request 400)
(define status-unauthorized 401)
(define status-payment-required 402)
(define status-forbidden 403)
(define status-not-found 404)
(define status-method-not-allowed 405)
(define status-not-acceptable 406)
(define status-proxy-authentication-required 407)
(define status-request-timeout 408)
(define status-conflict 409)
(define status-gone 410)
(define status-length-required 411)
(define status-precondition-failed 412)
(define status-request-entity-too-large 413)
(define status-request-uri-too-long 414)
(define status-unsupported-media-type 415)
(define status-requested-range-not-satisfiable 416)
(define status-expectation-failed 417)
(define status-internal-server-error 500)
(define status-not-implemented 501)
(define status-bad-gateway 502)
(define status-service-unavailable 503)
(define status-gateway-timeout 504)
(define status-http-version-not-supported 505)

(define (display-eol)
  (display "\r\n"))

(define (display-header header value)
  (format #t "~a: ~a" header value)
  (display-eol))

(define content-types
  '((text . "text/plain")
    (html . "text/html")))

(define default-content-type
  (make-parameter 'text))

(define display-content-type
  (case-lambda
   (() (display-content-type (default-content-type)))
   ((content-type)
    (display-header
     "Content-type"
     (if (string? content-type)
         content-type
         (alist-ref content-types content-type))))))

(define default-xml-prolog
  (make-parameter "<?xml version= \"1.0\" encoding= \"UTF-8\"?>"))

(define display-xml-prolog
  (case-lambda
   (() (display-xml-prolog (default-xml-prolog)))
   ((xml-prolog) (display xml-prolog))))

(define default-doctype
  (make-parameter 'xhtml-1.1))

(define doctypes
  '((xhtml-1.1 . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")))

(define display-doctype
  (case-lambda
   (() (display-doctype (default-doctype)))
   ((doctype)
    (display
     (if (string? doctype)
         doctype
         (alist-ref doctypes doctype))))))

(define default-content-type-&c.
  (make-parameter 'xhtml))

(define content-type-&cs.
  `((xhtml
     . ,(lambda ()
          (display-content-type 'html)
          (display-eol)
          (display-xml-prolog)
          (display-doctype)))
    (text
     . ,(lambda ()
          (display-content-type 'text)
          (display-eol)))))

;; Content-type, eol, prolog, doctype, etc.
(define display-content-type-&c.
  (case-lambda
   (() (display-content-type-&c.
        (default-content-type-&c.)))
   ((content-type-&c.)
    ((alist-ref content-type-&cs. content-type-&c.)))))

(define default-status
  (make-parameter status-ok))

(define display-status
  (case-lambda
   (() (display-status (default-status)))
   ((status)
    (display-header "Status" status))))

(define statuses
  `((,status-moved-permanently
     . ,(lambda (location)
          (display-header "Location" location)))))

(define display-status-&c.
  (case-lambda
   (()
    (display-status-&c. (default-status)))
   ((status)
    (display-status-&c. status (default-content-type)))
   ((status content-type . rest)
    (display-status status)
    (apply (alist-ref/default statuses status void) rest)
    (display-content-type-&c. content-type))))

(define query-client-any
  @("Return the first client parameter (e.g. {get,post,cookie}-parameter)
corresponding to the key."
    (key "The key whose value to extract")
    (default "A default value if {{key}} doesn't exist")
    (@to "string"))
  (case-lambda
   ((query key)
    (alist-any (query-client query) key))
   ((query key default)
    (alist-any (query-client query) key default))))

(define query-client-all
  @("Return a list of client parameters (e.g. {get,post,cookie}-parameters) 
corresponding to the key."
    (key "The key whose value to extract")
    (default "A default value if {{key}} doesn't exist")
    (@to "list"))
  (case-lambda
   ((query key)
    (alist-all (query-client query) key))
   ((query key default)
    (alist-all (query-client query) key default))))

(define query-server-any
  @("Return the first client parameter (e.g.
environment-variable) corresponding to the key."
    (key "The key whose value to extract")
    (default "A default value if {{key}} doesn't exist")
    (@to "string"))
  (case-lambda
   ((query key)
    (alist-any (query-server query) key))
   ((query key default)
    (alist-any (query-server query) key default))))

(define query-server-all
  @("Return a list of client parameters (e.g.
 environment-variables) corresponding to the key."
    (key "The key whose value to extract")
    (default "A default value if {{key}} doesn't exist")
    (@to "list"))
  (case-lambda
   ((query key)
    (alist-all (query-server query) key))
   ((query key default)
    (alist-all (query-server query) key default))))

(define (query-promiscuous query)
  #;(append-map cdr (query->alist query))
  (append (query-server query) (query-client query)))

(define query-any
  @("Return the first client or server parameter (see above) corresponding to the key."
    (key "The key whose value to extract")
    (default "A default value if {{key}} doesn't exist")
    (@to "string"))
  (case-lambda
   ((query key)
    (alist-any (query-promiscuous query) key))
   ((query key default)
    (alist-any (query-promiscuous query) key default))))

(define query-all
  @("Return a list of client or server parameters (see above) corresponding to the key."
    (key "The key whose value to extract")
    (default "A default value if {{key}} doesn't exist")
    (@to "list"))
  (case-lambda
   ((query key)
    (alist-all (query-promiscuous query) key))
   ((query key default)
    (alist-all (query-promiscuous query) key default))))

(define alist-any
  (case-lambda
   ((alist key)
    (alist-any alist key #f))
   ((alist key default)
    (alist-ref/default alist key #f))))

(define alist-all
  (case-lambda
   ((alist key)
    (alist-all alist key '()))
   ((alist key default)
    (fold (lambda (elt acc)
            (cons (cdr elt) acc))
          default
          (filter (lambda (pair) (equal? (car pair) key))
                  alist)))))

(define (env-string->symbol string)
  (string->symbol
   (string-downcase (string-substitute "_" "-" string #t))))

(define (call-with-dynamic-fastcgi-query quaerendum)
  @("Start a dynamic FastCGI server where output is bound to stdout;
and where a monadic function taking a query-record is called for every
request."
    (quaerendum "A monadic function receiving a query parameter")
    (@example-no-eval
     "An authorization server"
     (call-with-auth-db
      (lambda (connection)
        (call-with-dynamic-fastcgi-query
         (lambda (query)
           (let ((user (query-server-any query 'remote-user))
                 (password (query-server-any query 'remote-passwd)))
             (let ((status
                    (if (valid? connection user password "physician")
                        status-ok
                        status-unauthorized)))
               (display-status-&c. status)))))))))
  (fcgi-dynamic-server-accept-loop
   (lambda (in out err env)
     (let ((environment
            (map
             (match-lambda ((key . value)
                       (cons (env-string->symbol key)
                             value)))
             (env)))
           (cookies
            (form-urldecode
             (let ((cookies
                    (string-delete
                     char-set:whitespace
                     (env "HTTP_COOKIE" ""))))
               (and (not (string-null? cookies))
                    cookies))))
           (cookies2
            (form-urldecode
             (let ((cookies
                    (string-delete
                     char-set:whitespace
                     (env "HTTP_COOKIE2" ""))))
               (and (not (string-null? cookies))
                    cookies))))
           (post-data
            (form-urldecode
             (fcgi-get-post-data in env)))
           (query
            (form-urldecode
             (let ((query (env "QUERY_STRING")))
               (and (not (string-null? query))
                    query))))) 
       (parameterize
           ((current-output-port
             (make-output-port
              (lambda (scribendum)
                (out scribendum))
              void))
            (current-error-port
             (make-output-port
              (lambda (errandum)
                (err errandum))
              void)))
         (quaerendum (make-query environment
                                 (append cookies
                                         cookies2
                                         post-data
                                         query))))))))
