(module
 call-with-query
 (call-with-dynamic-fastcgi-query
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
  default-status
  display-status
  statuses
  display-status-&c.)
 (import scheme chicken posix)

 (use fastcgi
      call-with-environment-variables
      ports
      uri-common
      alist-lib
      srfi-1
      srfi-13
      format
      debug
      matchable)

 (define (query-any query key)
   (alist-ref/default query key #f))
 
 (define (query-all query key)
   (fold (lambda (elt acc)
           (cons (cdr elt) acc))
         '()
         (filter (lambda (pair) (equal? (car pair) key))
                 query)))

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
  (make-parameter 200))

(define display-status
  (case-lambda
   (() (display-status (default-status)))
   ((status)
    (display-header "Status" status))))

(define statuses
  `((200
     . ,(lambda content-type
          (let ((content-type (if (null? content-type)
                                  (default-content-type)
                                  (car content-type))))
            (display-content-type-&c. content-type))))
    (301
     . ,(lambda (location)
          (display-header "Location" location)
          (display-content-type-&c. 'text)))))

(define display-status-&c.
  (case-lambda
   (() (display-status-&c. (default-status)))
   ((status . rest)
    (display-status status)
    (apply (alist-ref/default statuses status void) rest))))

 (define (call-with-dynamic-fastcgi-query quaerendum)
   (fcgi-dynamic-server-accept-loop
    (lambda (in out err env)
      (let ((query
             (append
              (form-urldecode
               (fcgi-get-post-data in env))
              (form-urldecode
               (let ((query (env "QUERY_STRING")))
                 (and (not (string-null? query))
                      query))))))
        (parameterize
         ((current-output-port
           (make-output-port
            (lambda (scribendum)
              (out scribendum))
            void))
          ;; Redirecting current-error-port is actually a pain: it
          ;; obscures Apache logs.
          #;
          (current-error-port
           (make-output-port
            (lambda (errandum)
              (err errandum))
            void)))
          (call-with-environment-variables
           (env)
           (lambda ()
             (quaerendum query)))))))))