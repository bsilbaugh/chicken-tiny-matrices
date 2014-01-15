;;;
;;; Tiny Matrices Test Suite
;;;

(use test)
(use tiny-vectors)
(use tiny-matrices)

(module fp-op *
  (import scheme chicken)
  (define E+ fp+)
  (define E- fp-)
  (define E* fp*)
  (define E/ fp/)
  (define Esqrt fpsqrt))

(module fp-zo *
  (import scheme chicken)
  (define one 1.0)
  (define zero 0.0))

(module tv = (tiny-vectors fp-op))
(module tm = (tiny-matrices tv fp-zo))
(import tm)

(test-group "check 1x3 bindings to triple"
  (test-assert (= 0.1 
				  (3x1-elem-11 (3x1 0.1 0.2 0.3))
				  (e11<-3x1 (3x1 0.1 0.2 0.3))))
  (test-assert (= 0.2 
				  (3x1-elem-21 (3x1 0.1 0.2 0.3))
				  (e21<-3x1 (3x1 0.1 0.2 0.3))))
  (test-assert (= 0.3 
				  (3x1-elem-31 (3x1 0.1 0.2 0.3))
				  (e31<-3x1 (3x1 0.1 0.2 0.3))))
  (test-assert (equal? (3x1 1.1 2.2 3.3)
					   (3x1+3x1 (3x1 1.0 2.0 3.0)
								(3x1 0.1 0.2 0.3))))
  (test-assert (equal? (3x1 0.9 1.8 2.7)
					   (3x1-3x1 (3x1 1.0 2.0 3.0)
								(3x1 0.1 0.2 0.3))))
  (test-assert (equal? (3x1 0.5 0.25 0.125)
					   (3x1*sca (3x1 1.0 0.5 0.25) 0.5)))
  (test-assert (equal? (3x1 0.5 0.25 0.125)
					   (sca*3x1 0.5 (3x1 1.0 0.5 0.25))))
  (test-assert (equal? (3x1 2.0 1.0 0.5)
					   (3x1/sca (3x1 1.0 0.5 0.25) 0.5))))

(test-group "check 1x3 bindings to triple"
  (test-assert (= 0.1 
				  (1x3-elem-11 (1x3 0.1 0.2 0.3))
				  (e11<-1x3 (1x3 0.1 0.2 0.3))))
  (test-assert (= 0.2 
				  (1x3-elem-12 (1x3 0.1 0.2 0.3))
				  (e21<-1x3 (1x3 0.1 0.2 0.3))))
  (test-assert (= 0.3 
				  (1x3-elem-13 (1x3 0.1 0.2 0.3))
				  (e31<-1x3 (1x3 0.1 0.2 0.3))))
  (test-assert (equal? (1x3 1.1 2.2 3.3)
					   (1x3+1x3 (1x3 1.0 2.0 3.0)
								(1x3 0.1 0.2 0.3))))
  (test-assert (equal? (1x3 0.9 1.8 2.7)
					   (1x3-1x3 (1x3 1.0 2.0 3.0)
								(1x3 0.1 0.2 0.3))))
  (test-assert (equal? (1x3 0.5 0.25 0.125)
					   (1x3*sca (1x3 1.0 0.5 0.25) 0.5)))
  (test-assert (equal? (1x3 0.5 0.25 0.125)
					   (sca*1x3 0.5 (1x3 1.0 0.5 0.25))))
  (test-assert (equal? (1x3 2.0 1.0 0.5)
					   (1x3/sca (1x3 1.0 0.5 0.25) 0.5))))

(test-group "3x3 element access"
  (test-assert (= 1.1 
				  (3x3-elem-11 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e11<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 1.2 
				  (3x3-elem-12 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e12<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 1.3 
				  (3x3-elem-13 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e13<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 2.1 
				  (3x3-elem-21 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e21<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 2.2 
				  (3x3-elem-22 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e22<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 2.3 
				  (3x3-elem-23 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e23<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 3.1 
				  (3x3-elem-31 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e31<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 3.2 
				  (3x3-elem-32 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e32<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))
  (test-assert (= 3.3 
				  (3x3-elem-33 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))
				  (e33<-3x3 (3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))))


(test-group "3x3 row access"
  (test-assert (equal? (1x3 1.1 1.2 1.3)
					   (3x3-row-1 (3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (1x3 1.1 1.2 1.3)
					   (r1<-3x3 (3x3 
								 1.1 1.2 1.3 
								 2.1 2.2 2.3 
								 3.1 3.2 3.3))))
  (test-assert (equal? (1x3 2.1 2.2 2.3)
					   (3x3-row-2 (3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (1x3 2.1 2.2 2.3)
					   (r2<-3x3 (3x3 
								 1.1 1.2 1.3 
								 2.1 2.2 2.3 
								 3.1 3.2 3.3))))
  (test-assert (equal? (1x3 3.1 3.2 3.3)
					   (3x3-row-3 (3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (1x3 3.1 3.2 3.3)
					   (r3<-3x3 (3x3 
								 1.1 1.2 1.3 
								 2.1 2.2 2.3 
								 3.1 3.2 3.3)))))

(test-group "3x3 column access"
  (test-assert (equal? (3x1 1.1 2.1 3.1)
					   (3x3-col-1 (3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (3x1 1.1 2.1 3.1)
					   (c1<-3x3 (3x3 
								 1.1 1.2 1.3 
								 2.1 2.2 2.3 
								 3.1 3.2 3.3))))
  (test-assert (equal? (3x1 1.2 2.2 3.2)
					   (3x3-col-2 (3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (3x1 1.2 2.2 3.2)
					   (c2<-3x3 (3x3 
								 1.1 1.2 1.3 
								 2.1 2.2 2.3 
								 3.1 3.2 3.3))))
  (test-assert (equal? (3x1 1.3 2.3 3.3)
					   (3x3-col-3 (3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (3x1 1.3 2.3 3.3)
					   (c3<-3x3 (3x3 
								 1.1 1.2 1.3 
								 2.1 2.2 2.3 
								 3.1 3.2 3.3)))))


; --- LEFT OFF AT VECTOR OPERATIONS ---



