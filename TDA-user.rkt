#lang racket
;CONSTRUCTOR

(define createUser
  (lambda (username password)
    (list username password)))

(define user?
  (lambda (user)
    (if (and (string? (car user)) (string? (cadr user)) (null? (cddr user)))
        #t
        #f)))

(define getUsername
  (lambda (user)
    (if (user? user)
        (car user)
        #f)))

(define getPassword
  (lambda (user)
    (if (user? user)
        (cadr user)
        #f)))

(define modUsername
  (lambda (user newUsername)
    (if (user? user)
        (cons newUsername (cdr user))
        #f)))

(define modPassword
  (lambda (user newPassword)
    (if (user? user)
        (list (car user) newPassword)
        #f)))

(provide (all-defined-out))