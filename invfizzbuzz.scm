#!/usr/local/bin/gosh
(use srfi-42)

(define strl '(fizz buzz fizz fizz buzz fizz fizzbuzz))
(define intl '(3 5 6 9 10 12 15))

(define (min-list lst)
  (let loop ((ls (map (lambda(x)(length x)) lst))
			 (n 0))
	(if (eq? (car ls) (apply min ls))
	  n
	  (loop (cdr ls) (+ n 1)))))

(define (fizzbuzz lst)
  (let loop ((n 1)
			 (ls lst)
			 (str strl)
			 (int intl))
;	(print "fizzbuzz's lst: "lst);lst
;	(print "fizzbuzz's n: "n);n
;	(print "fizzbuzz's str: "str);str
;	(print "fizzbuzz's int: "int);int
	(cond
	  ((null? (cdr ls))(solve lst str int))
	  ((eq? (car ls) 'fizzbuzz)
	   (loop (+ n 1) (cdr ls) (append str strl) (append int (map (lambda(x)(* (+ 15 x) n)) intl))))
	  (else (loop n (cdr ls) str int)))))

(define (solve lst str int)
  (let loop ((str str)
			 (int int)
			 (result '()))
;	(print "solve's lst: "lst);lst
;	(print "solve's str: "str);str
;	(print "solve's int: "int);int
;	(print "solve's result: "result);result
	(cond ((> (length lst) (length str))
		   (reverse (map (lambda(x)(list-ec (:range v (car x) (+ (last x) 1)) v)) result)))
		  ((equal? lst (take str (length lst)))
		   (loop (cdr str) (cdr int) (cons (take int (length lst)) result)))
		  (else (loop (cdr str) (cdr int) result)))))

(define (invfizzbuzz lst)
  (let ((result (fizzbuzz lst)))
;	(print "result is "result);result
	(if (null? result)
	  '()
	  (print (list-ref result (min-list result))))))
