#lang racket

(require "TDA-user.rkt")

;CONSTRUCTOR

(define createUserList
  (lambda (username password)
    (list (createUser username password))))


;PERTENENCIA

(define userList?
  (lambda (userList)
    (if (null? userList)
        #t
        (if (user? (car userList))
            (userList? (cdr userList))
            #f))))

;SELECTOR

(define getFirstUser
  (lambda (userList)
  (if (userList? userList)
      (car userList)
      #f)))

(define GUBURec
  (lambda (userList username)
    (if (null? userList)
        #f
        (if (equal? (getUsername (car userList)) username)
            (car userList)
            (GUBURec (cdr userList) username)))))
        

(define getUserByUsername
  (lambda (userList username)
    (if (and (userList? userList) (string? username))
        (GUBURec userList username)
        #f)))

(define getLastUser
  (lambda (userList)
    (if (userList? userList)
        (if (null? (cdr userList))
            (car userList)
            (getLastUser (cdr userList)))
        #f)))

;MODIFIER

;DEBE SER PRECEDIDA POR UNA FUNCION "USERLIST?" ANTES DE SU LLAMADO.

(define usernameOnUL?
  (lambda (userList user)
    (if (null? userList)
        #f
        (if (equal? (getUsername user) (getUsername (car userList)))
            #t
            (usernameOnUL? (cdr userList) user)))))
        
(define newUserOnUserlist
  (lambda (userList user)
    (if (and (userList? userList) (not (usernameOnUL? userList user)))
        (cons user userList)
        "EL USUARIO YA EXISTE.")))

;DEBE SER PRECEDIDA POR UNA FUNCION "USERLIST?" ANTES DE SU LLAMADO.
;RECURSION DE FUNCION DE BORRADO

(define DUBURec
  (lambda (userList username)
    (if (null? userList)
        '()
        (if (equal? username (getUsername (car userList)))
            (DUBURec (cdr userList) username)
            (cons (car userList) (DUBURec (cdr userList) username))))))

(define delUserByUsername
  (lambda (userList username)
    (if (userList? userList)
        (DUBURec userList username)
        #f)))

;DEBE SER PRECEDIDA POR UNA FUNCION "USERLIST?" ANTES DE SU LLAMADO.
;RECURSION DE FUNCION DE CAMBIO DE CONTRASEÑA
     
(define CUPRec
  (lambda (userList username oldPassword newPassword)
    (if (null? userList)
        '()
        (if (equal? (getUsername (car userList)) username)
            (if (equal? oldPassword (getPassword (car userList)))
                (cons (modPassword (car userList) newPassword) (cdr userList))
                "CONTRASEÑA INCORRECTA.")
            (cons (car userList) (CUPRec (cdr userList) username oldPassword newPassword))))))
   

(define changeUserPassword
  (lambda (userList username oldPassword newPassword)
    (if (userList? userList)
        (CUPRec userList username oldPassword newPassword)
        #f)))

(provide (all-defined-out))