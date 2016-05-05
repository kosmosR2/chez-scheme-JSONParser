(define NUMBER_CHAR '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.))

(define charGetter
  (lambda (text)
    (lambda (pointer)
      (string-ref text pointer))))
 

;(define (findFristChar json start)
;  (let ((current-char (string-ref json start))
;        (next-pointer (+ 1 start))
;        (increse-line
;         (lambda (target-list)
;           (list (car target-list)
;                 (+ 1 (car (cdr target-list)))))))
;    (cond ((equal? current-char #\space)
;           (findFristChar json next-pointer))
;          ((equal? current-char #\newline)
;           (increse-line (findFristChar json next-pointer)))
;          (else start))))

(define (getValue JSON start)
  (let ((current-char (string-ref JSON start)))
    (cond ((equal? current-char #\")
           (load-string JSON start))
          ((equal? (find (lambda (n)
                           (equal? current-char n))
                         NUMBER_CHAR) current-char)
           (load-number JSON start))
          ((equal? current-char #\[)
           (load-array JSON start))
          ((or (equal? current-char #\t)
               (equal? current-char #\f)
               (equal? current-char #\n))
           (load-bool-null JSON start))
          ((equal? current-char #\{)
           (load-json JSON start))
          ((or (equal? current-char #\newline)
               (equal? current-char #\space))
           (getValue JSON (+ 1 start)))
          (else (raise `(not-parseable ,start))))))

(define (load-array JSON start)
  (define reader (charGetter JSON))
  (define read-eof
    (lambda (pointer)
      (let ((current-char (reader pointer)))
        (cond ((equal? current-char #\]) '())
              ((or (equal? current-char #\space)
                   (equal? current-char #\newline)
                   (equal? current-char #\,))
               (read-eof (+ 1 pointer)))
              (else (let ((current-value (getValue JSON pointer)))
                      (cons
                       (car current-value)
                       (read-eof (+ 1 (cadr current-value))))))))))
  (if (equal? (reader start) #\[)
      (read-eof (+ 1 start))
      (raise `(not-parseable ,start))))

(load-array "[1,2,3,4]" 0)
      
               
              

(define (load-string JSON start)
  (define reader (charGetter JSON))
  (define-syntax toeof
    (syntax-rules ()
      ((_ target-pointer append-list)
       (let ((eof (readUntillEOF target-pointer)))
         (list
          (string-append
           (apply string append-list)
           (car eof))
          (cadr eof))))))
  (define readUntillEOF
    (lambda (pointer)
      (let ((current-char (reader pointer)))
        (cond ((equal? current-char #\\)
               (let ((next-char (reader (+ 1 pointer))))
                 (if (equal? next-char #\")
                     (toeof (+2 pointer) `(#\" ,next-char))
                     (toeof (+ 1 pointer) '(#\\)))))
              ((equal? current-char #\") (list "" pointer))
              (else (toeof (+ 1 pointer) `(,current-char)))))))
  (if (equal? (reader start) #\")
      (readUntillEOF (+ 1 start))
      (raise "not a string" start)))


(define load-number
  (lambda (JSON start)
    (define reader (charGetter JSON))
    (define read-eof
      (lambda (pointer)
        (let ((current-char (reader pointer)))
          (cond ((equal?
                  (find (lambda (n)
                          (equal? current-char n)) NUMBER_CHAR)
                  current-char)
                 (let ((eof (read-eof (+ 1 pointer))))
                   (list
                    (string-append
                     (string current-char)
                     (car eof))
                    (if (equal? #\. current-char)
                        (if (not (cadr eof))
                            #t
                            (raise `(not-parseable ,pointer)))
                        (cadr eof))
                    (caddr eof))))
                ((or (equal? current-char #\space)
                     (equal? current-char #\,)
                     (equal? current-char #\newline)
                     (equal? current-char #\])
                     (equal? current-char #\})
                     (equal? current-char 'eof))
                 (list "" #f (- pointer 1)))
                (else (raise `(not-parseable ,pointer)))))))   
    (let ((result (read-eof start)))
      (list (string->number (car result)) (caddr result)))))

(define load-bool-null
  (lambda (JSON start)
    (define reader (charGetter JSON))
    (define (read-eof pointer expect)
      (let ((current-char (reader pointer)))
        (if (> (length expect) 0)
            (if (equal? current-char (car expect))
                (read-eof (+ 1 pointer) (cdr expect))
                (raise `(not-parseable ,pointer)))
            (if (or (equal? current-char #\newline)
                    (equal? current-char #\space)
                    (equal? current-char #\,))
                (- pointer 1)
                (raise `(not-parseable ,pointer))))))
    (let ((frist-char (reader start)))
      (cond ((equal? frist-char #\f)
             (list #f (read-eof start '(#\f #\a #\l #\s #\e))))
            ((equal? frist-char #\t)
             (list #t (read-eof start '(#\t #\r #\u #\e))))
            ((equal? frist-char #\n)
             (list '() (read-eof start '(#\n #\u #\l #\l))))
            (else (raise `(not-parseable ,start)))))))


      
      


(define JSONexample "{
\"number\":233,
\"string\":\"test\",
\"float\":2.33,
\"bool\":false,
\"null\":null,
\"array\":[\"1\",2,2.33],
\"obj\":{
  \"t\":\"test\"
}}\n")


