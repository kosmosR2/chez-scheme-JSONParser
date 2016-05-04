(define NUMBER_CHAR '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define charGetter
  (lambda (text)
    (lambda (pointer)
      (string-ref text pointer))))

(define (throw-error errorMessage)
  (raise (condition
          (make-error)
          (make-message-condition errorMessage))))
 

(define (findFristChar json start)
  (let ((current-char (string-ref json start))
        (next-pointer (+ 1 start))
        (increse-line (lambda (target-list)
                        (list (car target-list) (+ 1 (car (cdr target-list)))))))
    (cond ((equal? current-char #\space) (findFristChar json next-pointer))
          ((equal? current-char #\newline)
           (increse-line (findFristChar json next-pointer)))
          (else (list start 1)))))

(define (getValue JSON start)
  (let ((current-pointer (string-ref json start)))
    (cond ((equal? current-pointer #\") (loadString JSON start))
          ((equal? current-pointer #\[) (loadArray JSON start))
          ((or (equal? current-pointer #\t)
               (equal? current-pointer #\f)) (loadBool JSON start))
          ((equal? current-pointer #\n)  (loadNull JSON start))
          ((equal? current-pointer #\{) (loadJSON JSON start))
          ((not (equal? (find (lambda (n)
                                (equal? current-pointer n))
                              NUMBER_CHAR) #f) (loadNumber JSON start)))
          (else (raise
                 (condition
                  (make-error)
                  (make-message-condition "not a parseable JSON")))))))

(define (loadString JSON start)
  (define reader (charGetter JSON))
  (define readUntillEOF (lambda (pointer)
                          (let ((current-char (reader pointer)))
                            (cond ((equal? current-char #\\) (let ((next-char (reader (+ 1 pointer))))
                                                               (if (equal? next-char #\")
                                                                   (string-append (string #\" next-char)
                                                                           (readUntillEOF (+ 2 pointer)))
                                                                   (string-append (string #\\)
                                                                           (readUntillEOF (+ 1 pointer))))))
                                  ((equal? current-char #\") "")
                                  (else (string-append (string current-char)
                                                (readUntillEOF (+ 1 pointer))))))))
  (if (equal? (reader start) #\")
      (readUntillEOF (+ 1 start))
      (throw-error "not a string")))


(define (load-number)
  






                       
        


   


           
          
           
               
        
        


()

      
      


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

(display (JSONparse JSONexample))




(substring "abcde" 1 5)

(begin (display "233")
       233)
