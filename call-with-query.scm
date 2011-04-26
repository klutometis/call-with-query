(module
 call-with-query
 (call-with-dynamic-fastcgi-query
  display-eol
  display-header
  default-content-type
  display-content-type
  default-xml-prolog
  display-xml-prolog
  default-doctype
  display-doctype)
 (import scheme chicken posix)

 (use fastcgi
      call-with-environment-variables
      ports
      uri-common
      alist-lib
      srfi-13
      format
      debug)

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
          (alist-ref/default
           content-types
           content-type
           (default-content-type)))))))

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
          (alist-ref/default doctypes doctype (default-doctype)))))))

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