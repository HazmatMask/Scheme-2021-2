#lang racket

;DATE
;CONSTRUCTOR
(define createDate
  (lambda (day month year)
    (list day month year)))

;PERTENENCIA

(define date?
  (lambda (date)
    (if (and
          (integer? (caddr date)) ;AÑO
          (integer? (cadr date)) (> (cadr date) 0) (< (cadr date) 13) ;MES
          (integer? (car date)) (> (cadr date) 0) ;DIA
          (if (or (= (cadr date) 1) (= (cadr date) 3) (= (cadr date) 5) (= (cadr date) 7)
                  (= (cadr date) 8) (= (cadr date) 10) (= (cadr date) 12)) ;31 DIAS
              (if (< (car date) 32)
                  #t #f)
              (if (= (cadr date) 2) ;FEBRERO
                  (if (= (modulo (caddr date) 4) 0)
                      (if (= (modulo (caddr date) 100) 0)
                          (if (= (modulo (caddr date) 400) 0)
                              (if (< (car date) 30) ;AÑO BISIESTO
                                  #t #f)
                              (if (< (car date) 29) ;AÑO NO BISIESTO
                                  #t #f))
                      (if (< (car date) 30) ;AÑO BISIESTO
                                  #t #f))                   
                      (if (< (car date) 29) ;AÑO NO BISIESTO
                          #t #f))
                  (if (< (car date) 31) ;30 DIAS
                      #t #f)))
          (null? (cdddr date)))
         #t #f)))

;SELECTORES
(define getDay
  (lambda (date)
    (car date)))

(define getMnth
  (lambda (date)
    (cadr date)))

(define getYear
  (lambda (date)
    (caddr date)))

;MODIFICADORES

(define modDay
  (lambda (date day)
    (if (date? (cons day (cdr date)))
        (cons day (cdr date))
        date)))

(define modMnth
  (lambda (date month)
    (if (date? (cons (car date) (cons month (cddr date))))
        (cons (car date) (cons month (cddr date)))
        date)))

(define modYear
  (lambda (date year)
    (if (date? (list (car date) (cadr date) year))
        (list (car date) (cadr date) year)
        date)))

;;crear suma de fechas

(provide (all-defined-out))