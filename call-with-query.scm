(module
 call-with-query
 (call-with-dynamic-fastcgi-query
  query->alist
  query-client-any
  query-client-all
  query-server-any
  query-server-all
  query-any
  query-all
  display-eol
  display-header
  content-types
  default-content-type
  display-content-type
  default-xml-prolog
  display-xml-prolog
  doctypes
  default-doctype
  display-doctype
  content-type-&cs.
  default-content-type-&c.
  display-content-type-&c.
  status-continue
  status-switching-protocols
  status-ok
  status-created
  status-accepted
  status-non-authoritative-information
  status-no-content
  status-reset-content
  status-partial-content
  status-multiple-choices
  status-moved-permanently
  status-found
  status-see-other
  status-not-modified
  status-use-proxy
  status-unused
  status-temporary-redirect
  status-bad-request
  status-unauthorized
  status-payment-required
  status-forbidden
  status-not-found
  status-method-not-allowed
  status-not-acceptable
  status-proxy-authentication-required
  status-request-timeout
  status-conflict
  status-gone
  status-length-required
  status-precondition-failed
  status-request-entity-too-large
  status-request-uri-too-long
  status-unsupported-media-type
  status-requested-range-not-satisfiable
  status-expectation-failed
  status-internal-server-error
  status-not-implemented
  status-bad-gateway
  status-service-unavailable
  status-gateway-timeout
  status-http-version-not-supported
  default-status
  display-status
  statuses
  display-status-&c.)
 (import scheme chicken posix)

 (use fastcgi
      ports
      uri-common
      alist-lib
      srfi-1
      srfi-13
      srfi-14
      format
      debug
      matchable
      regex
      defstruct)

 (defstruct
   query
   server
   client)

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
   (case-lambda
    ((query key)
     (alist-any (query-client query) key))
    ((query key default)
     (alist-any (query-client query) key default))))

 (define query-client-all
   (case-lambda
    ((query key)
     (alist-all (query-client query) key))
    ((query key default)
     (alist-all (query-client query) key default))))

 (define query-server-any
   (case-lambda
    ((query key)
     (alist-any (query-server query) key))
    ((query key default)
     (alist-any (query-server query) key default))))

 (define query-server-all
   (case-lambda
    ((query key)
     (alist-all (query-server query) key))
    ((query key default)
     (alist-all (query-server query) key default))))

 (define (query-promiscuous query)
   (append-map cdr (query->alist query)))

 (define query-any
   (case-lambda
    ((query key)
     (alist-any (query-promiscuous query) key))
    ((query key default)
     (alist-any (query-promiscuous query) key default))))

 (define query-all
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
           void)))
        (quaerendum (make-query server: environment
                                client: (append cookies
                                                cookies2
                                                post-data
                                                query)))))))))