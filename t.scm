(define NUMBER_CHAR '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.))
(define charGetter
  (lambda (text)
    (lambda (pointer)
      (string-ref text pointer))))

(define (throw-error errorMessage)
  (raise (condition
          (make-error)
          (make-message-condition errorMessage))))

(define load-number
  (lambda (JSON start)
    (define reader (charGetter JSON))
    (define read-eof
      (lambda (pointer)
        (let ((current-char (reader pointer)))
          (cond ((equal? (find (lambda (n)
                                 (current-char)) NUMBER_CHAR) current-char)
                 (let ((eof (read-eof (+ 1 pointer))))
                   (list (append (string current-char) (car eof))
                         (if (equal? #\. current-char)
                             (if (not (cadr eof))
                                 #t
                                 (throw-errorr "not a parseable Number"))
                             (cadr eof))
                         (caddr eof))))
                ((or (equal? ))
                
                   
                             
                                 
                )))))







                              


