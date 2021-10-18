#lang racket

;CONSTRUCTOR
;NOMBRE AUTOR PERMISO_EDICION PERMISO_LECTURA PERMISO_COMENTARIOS TEXTO

(define createDocument
  (lambda (name author)
    (list name author '() '() '() "")))

;PERTENENCIA

(define list_of_permissions?
  (lambda (element)
    (if (list? element)
        (if (null? element)
            #t
            (list_of_permissions? (cdr element)))
        #f)))

(define document?
  (lambda (document)
    (if (and (string? (car document))
             (string? (cadr document))
             (list_of_permissions? (caddr document))
             (list_of_permissions? (cadddr document))
             (list_of_permissions? (car (cddddr document)))
             (string? (cadr (cddddr document)))
             (null? (cddr (cddddr document))))
        #t
        #f)))

;SELECTOR

(define getName
  (lambda (document) (car document)))

(define getAuthor
  (lambda (document) (cadr document)))

(define getEditors
  (lambda (document) (caddr document)))

(define getReaders
  (lambda (document) (cadddr document)))

(define getCommenters
  (lambda (document) (car (cddddr document))))

(define getText
  (lambda (document) (cadr (cddddr document))))

;MODIFIERS

(define modName
  (lambda (document newElement) (cons newElement (cdr document))))

(define modAuthor
  (lambda (document newElement) (cons (car document) (cons newElement (cddr document)))))

(define modEditors
  (lambda (document newElement)
    (cons (car document) (cons (cadr document) (cons newElement (cdddr document))))))

(define modReaders
  (lambda (document newElement)
    (cons (car document) (cons (cadr document) (cons (caddr document) (cons newElement (cddddr document)))))))

(define modCommenters
  (lambda (document newElement)
    (cons (car document)
          (cons (cadr document)
                (cons (caddr document)
                      (cons (cadddr document)
                            (cons newElement (cdr (cddddr document)))))))))

(define modText
  (lambda (document newElement)
    (cons (car document)
          (cons (cadr document)
                (cons (caddr document)
                      (cons (cadddr document)
                            (cons (car (cddddr document))
                                  (cons newElement '()))))))))

(provide (all-defined-out))