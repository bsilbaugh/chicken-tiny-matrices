;;;
;;; Tiny Matrices
;;;
;;; An implementation of 1x3, 3x1, and 3x3 matricies.
;;;
;;; Copyright 2014 Benjamin Silbaugh

(functor (tiny-matrices 
		  (M (make-vec 
			  vec-elem-1 vec-elem-2 vec-elem-3
			  vec+vec vec-vec vec*sca sca*vec vec/sca
			  E+ E- E* E/ Esqrt)) (N (zero one))) *

  (import scheme chicken)
  (import M N)
  (use tuples)

;;; Define 1x3 matrix as a 3 vector (extension of triple)

  (define make-3x1 make-vec)
  (define 3x1-elem-11 vec-elem-1)
  (define 3x1-elem-21 vec-elem-2)
  (define 3x1-elem-31 vec-elem-3)
  (define 3x1+3x1 vec+vec)
  (define 3x1-3x1 vec-vec)
  (define 3x1*sca vec*sca)
  (define sca*3x1 sca*vec)
  (define 3x1/sca vec/sca)

  ;; Define alias for constructor
  (define 3x1 make-3x1)
  (define e11<-3x1 3x1-elem-11)
  (define e21<-3x1 3x1-elem-21)
  (define e31<-3x1 3x1-elem-31)

;;; Define a 3x1 matrix as a 3 vector (extension of triple)

  (define make-1x3 make-vec)
  (define 1x3-elem-11 vec-elem-1)
  (define 1x3-elem-12 vec-elem-2)
  (define 1x3-elem-13 vec-elem-3)
  (define 1x3+1x3 vec+vec)
  (define 1x3-1x3 vec-vec)
  (define 1x3*sca vec*sca)
  (define sca*1x3 sca*vec)
  (define 1x3/sca vec/sca)

  ;; Define aliases for constructor and accessors
  (define 1x3 make-1x3)
  (define e11<-1x3 1x3-elem-11)
  (define e21<-1x3 1x3-elem-12)
  (define e31<-1x3 1x3-elem-13)

;;; Define 3X3 matrix as nonuple with row-major ordering

  (define make-3x3 make-nonuple)
  (define 3x3-elem-11 nonuple-elem-0)
  (define 3x3-elem-12 nonuple-elem-1)  
  (define 3x3-elem-13 nonuple-elem-2)  
  (define 3x3-elem-21 nonuple-elem-3)
  (define 3x3-elem-22 nonuple-elem-4)
  (define 3x3-elem-23 nonuple-elem-5)
  (define 3x3-elem-31 nonuple-elem-6)
  (define 3x3-elem-32 nonuple-elem-7)
  (define 3x3-elem-33 nonuple-elem-8)

  ;; Define aliases for 3x3 matrix constructor and accessors
  (define 3x3 make-3x3)
  (define e11<-3x3 3x3-elem-11)
  (define e12<-3x3 3x3-elem-12)  
  (define e13<-3x3 3x3-elem-13)  
  (define e21<-3x3 3x3-elem-21)
  (define e22<-3x3 3x3-elem-22)  
  (define e23<-3x3 3x3-elem-23)  
  (define e31<-3x3 3x3-elem-31)
  (define e32<-3x3 3x3-elem-32)  
  (define e33<-3x3 3x3-elem-33)  


;;; Define row and column accessors (these should get inlined when compiled?)

  (define (3x3-row-1 a)
	(make-1x3 (3x3-elem-11 a) (3x3-elem-12 a) (3x3-elem-13 a)))

  (define (3x3-row-2 a)
	(make-1x3 (3x3-elem-21 a) (3x3-elem-22 a) (3x3-elem-23 a)))

  (define (3x3-row-3 a)
	(make-1x3 (3x3-elem-31 a) (3x3-elem-32 a) (3x3-elem-33 a)))

  (define (3x3-col-1 a)
	(make-3x1 (3x3-elem-11 a) (3x3-elem-21 a) (3x3-elem-31 a)))

  (define (3x3-col-2 a)
	(make-3x1 (3x3-elem-12 a) (3x3-elem-22 a) (3x3-elem-32 a)))
  
  (define (3x3-col-3 a)
	(make-3x1 (3x3-elem-13 a) (3x3-elem-23 a) (3x3-elem-33 a)))

  ;; Define analog aliases for row and col accessors as element accessors
  (define r1<-3x3 3x3-row-1)
  (define r2<-3x3 3x3-row-2)
  (define r3<-3x3 3x3-row-3)
  (define c1<-3x3 3x3-col-1)
  (define c2<-3x3 3x3-col-2)
  (define c3<-3x3 3x3-col-3)

;;; Define vector operations for 3x3

(define (3x3+3x3 a b)
  (nonuple-map-bfun E+ a b))

(define (3x3-3x3 a b)
  (nonuple-map-bfun E- a b))

(define (3x3*sca a c)
  (nonuple-map-ufun (lambda (aij) (E* aij c)) a))

(define (sca*3x3 c a) (3x3*sca a c))

(define (3x3/sca a c)
  (nonuple-map-ufun (lambda (aij) (E/ aij c)) a))

;;; Define matrix - matrix products

(define (3x3*3x3 a b)
  (let ((a1 (3x3-row-1 a)) (a2 (3x3-row-2 a)) (a3 (3x3-row-3))
		(b1 (3x3-col-1 b)) (b2 (3x3-col-2 b)) (b3 (3x3-col-3)))
  (make-3x3 (dot a1 b1) (dot a1 b2) (dot a1 b3)
			(dot a2 b1) (dot a2 b2) (dot a2 b3)
			(dot a3 b1) (dot a3 b2) (dot a3 b3))))

(define (3x3*3x3*3x3 a b c)
  (3x3*3x3 a (3x3*3x3 b c)))

;; Define matrix - vector products

(define (3x3*3x1 a v)
  (make-3x1 (dot (3x3-row-1 a) v)
			(dot (3x3-row-2 a) v)
			(dot (3x3-row-3 a) v)))

(define (1x3*3x3 v a)
  (make-1x3 (dot v (3x3-col-1 a))
			(dot v (3x3-col-2 a))
			(dot v (3x3-col-3 a))))

(define (1x3*3x3*3x1 u a v)
  (dot u (3x3*3x1 a v)))

;; Define vector - vector products

(define 1x3*3x1 dot)

(define (3x1*1x3 u v)
  (let ((u1 (3x1-elem-11 u)) (u2 (3x1-elem-21 u)) (u3 (3x1-elem-31 u))
		(v1 (1x3-elem-11 v)) (v2 (1x3-elem-12 v)) (v3 (1x3-elem-13 v)))
	(make-3x3 (* u1 v1) (* u1 v2) (* u1 v3)
			  (* u2 v1) (* u2 v2) (* u2 v3)
			  (* u3 v1) (* u3 v2) (* u3 v3))))

;;; Define unitary 3x3 operations

;; Matrix transposition
(define (3x3T a)
  (make-3x3 (3x3-elem-11 a) (3x3-elem-21 a) (3x3-elem-31 a)
			(3x3-elem-12 a) (3x3-elem-22 a) (3x3-elem-32 a)
			(3x3-elem-13 a) (3x3-elem-23 a) (3x3-elem-33 a)))

;;; Define useful identities and constructors

(define (diag3x3 e11 e22 e33)
  (make-3x3 e11   zero zero
			zero   e22 zero
			zero  zero  e33))

(define i3x3 (diag3x3 one one one))

);module
