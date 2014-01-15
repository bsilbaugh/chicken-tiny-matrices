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
  (test 0.1 (3x1-elem-11 (make-3x1 0.1 0.2 0.3)))
  (test 0.2 (3x1-elem-21 (make-3x1 0.1 0.2 0.3)))
  (test 0.3 (3x1-elem-31 (make-3x1 0.1 0.2 0.3)))
  (test-assert (equal? (make-3x1 1.1 2.2 3.3)
					   (3x1+3x1 (make-3x1 1.0 2.0 3.0)
								(make-3x1 0.1 0.2 0.3))))
  (test-assert (equal? (make-3x1 0.9 1.8 2.7)
					   (3x1-3x1 (make-3x1 1.0 2.0 3.0)
								(make-3x1 0.1 0.2 0.3))))
  (test-assert (equal? (make-3x1 0.5 0.25 0.125)
					   (3x1*sca (make-3x1 1.0 0.5 0.25) 0.5)))
  (test-assert (equal? (make-3x1 0.5 0.25 0.125)
					   (sca*3x1 0.5 (make-3x1 1.0 0.5 0.25))))
  (test-assert (equal? (make-3x1 2.0 1.0 0.5)
					   (3x1/sca (make-3x1 1.0 0.5 0.25) 0.5))))

(test-group "check 1x3 bindings to triple"
  (test 0.1 (1x3-elem-11 (make-1x3 0.1 0.2 0.3)))
  (test 0.2 (1x3-elem-12 (make-1x3 0.1 0.2 0.3)))
  (test 0.3 (1x3-elem-13 (make-1x3 0.1 0.2 0.3)))
  (test-assert (equal? (make-1x3 1.1 2.2 3.3)
					   (1x3+1x3 (make-1x3 1.0 2.0 3.0)
								(make-1x3 0.1 0.2 0.3))))
  (test-assert (equal? (make-1x3 0.9 1.8 2.7)
					   (1x3-1x3 (make-1x3 1.0 2.0 3.0)
								(make-1x3 0.1 0.2 0.3))))
  (test-assert (equal? (make-1x3 0.5 0.25 0.125)
					   (1x3*sca (make-1x3 1.0 0.5 0.25) 0.5)))
  (test-assert (equal? (make-1x3 0.5 0.25 0.125)
					   (sca*1x3 0.5 (make-1x3 1.0 0.5 0.25))))
  (test-assert (equal? (make-1x3 2.0 1.0 0.5)
					   (1x3/sca (make-1x3 1.0 0.5 0.25) 0.5))))

(test-group "3x3 element access"
  (test 1.1 (3x3-elem-11 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 1.2 (3x3-elem-12 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 1.3 (3x3-elem-13 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 2.1 (3x3-elem-21 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 2.2 (3x3-elem-22 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 2.3 (3x3-elem-23 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 3.1 (3x3-elem-31 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 3.2 (3x3-elem-32 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3)))
  (test 3.3 (3x3-elem-33 (make-3x3 1.1 1.2 1.3 2.1 2.2 2.3 3.1 3.2 3.3))))

(test-group "3x3 row access"
  (test-assert (equal? (make-1x3 1.1 1.2 1.3)
					   (3x3-row-1 (make-3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (make-1x3 2.1 2.2 2.3)
					   (3x3-row-2 (make-3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (make-1x3 3.1 3.2 3.3)
					   (3x3-row-3 (make-3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3)))))

(test-group "3x3 column access"
  (test-assert (equal? (make-3x1 1.1 2.1 3.1)
					   (3x3-col-1 (make-3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (make-3x1 1.2 2.2 3.2)
					   (3x3-col-2 (make-3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3))))
  (test-assert (equal? (make-3x1 1.3 2.3 3.3)
					   (3x3-col-3 (make-3x3 
								   1.1 1.2 1.3 
								   2.1 2.2 2.3 
								   3.1 3.2 3.3)))))

; --- LEFT OFF AT VECTOR OPERATIONS ---



