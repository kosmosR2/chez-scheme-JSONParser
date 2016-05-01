(define NUMBER_CHAR '(0 1 2 3 4 5 6 7 8 9))

(define charGetter
  (lambda (text)
    (lambda (pointer)
      (string-ref text pointer))))

(define JSONparse
  (lambda (json)
    (let ((line 1)
          (getChar (charGetter json)))
      
      )))

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
