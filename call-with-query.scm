(module
 call-with-query
 (call-with-dynmaci-fastcgi-query)
 (use fastcgi
      call-with-environment-variables
      ports
      srfi-39
      uri-common
      alist-lib)

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