#lang racket

(define slide_char
  (lambda (character number)
    (integer->char
     (modulo (+ number (char->integer character)) 1792))))

(define rec_slide_text
  (lambda (text number)
    (if (null? text)
        '()
    (cons
     (slide_char (car text) number)
     (rec_slide_text (cdr text) number)))))

(define slide_text
  (lambda (text number)
    (rec_slide_text text number)))

(provide (all-defined-out))