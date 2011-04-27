(use call-with-query test)

(test-group
 "Status &c."
 (test
  "Default 200 status"
  "Status: 200\r\nContent-type: text/plain\r\n\r\n"
  (with-output-to-string
    (lambda ()
      (display-status-&c.))))

 (test
  "301 redirect"
  "Status: 301\r\nLocation: http://example.com\r\nContent-type: text/plain\r\n\r\n"
  (with-output-to-string
    (lambda ()
      (display-status-&c. 301 'text "http://example.com")))))

(test-group
 "Query-lookup"
 (let ((query '((cancel . "Cancel")
                (doctor . 1)
                (doctor . 13)))
       (key 'cancel))
   (test
    "Multiple values, choose any"
    1
    (query-any query 'doctor))
   (test
    "Multiple values, choose all"
    '(13 1)
    (query-all query 'doctor))
   (test
    "No values, choose any"
    #f
    (query-any query 'harro))
   (test
    "No values, choose all"
    '()
    (query-all query 'harro))))

(test-exit)