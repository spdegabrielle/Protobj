#lang racket/base
;; $Id: test-protobj.rkt,v 1.3 2011/03/04 07:52:42 neilpair Exp $
;; See file protobj.rkt for legal info.

(require (planet neil/testeez:1:2)
         "protobj.rkt")

(testeez
 "Protobj"

 (test-define "Object \"a\""                     a (%))
 (test/equal  "\"a\" parent is root"  (eq? (^ a) (current-root-object)) #t)
 (test-eval   "Add to \"a\" slot \"x\" value 1"  (! a x 1))
 (test/equal  "\"a\" slot \"x\" is 1"            (? a x)                 1)
 (test-define "Object \"b\" clones \"a\""        b (% a))
 (test/equal  "\"b\" inherited slot \"x\" is 1"  (? b x)                 1)
 (test-eval   "Set \"a\" slot \"x\" to 42"       (! a x 42))
 (test/equal  "\"b\" slot \"x\" is now 42"       (? b x)                 42)
 (test-eval   "Set \"b\" slot \"x\" to 69"       (! b x 69))
 (test/equal  "\"b\" slot \"x\" is 69"           (? b x)                 69)
 (test/equal  "\"a\" slot \"x\" is still 42"     (? a x)                 42)

 (test-eval "Add to object \"a\" an \"xplus\" slot containing a method"
            (! a xplus (lambda ($ n) (+ (? $ x) n))))

 (test/equal "42 + 7 = 49" (@ a xplus 7) 49)
 (test/equal "69 + 7 = 76" (@ b xplus 7) 76)

 (test/equal "42 + 7 = 49" (@ a (xplus 1000) (xplus 7)) 49)

 (test-define "Object \"c\" clones \"a\", adds slot \"y\""
              c (% a (y 101)))
 (test/equal "\"c\" slot \"x\" is 42"  (? c x) 42)
 (test/equal "\"c\" slot \"y\" is 101" (? c y) 101)

 (test-define "Object \"d\" clones \"a\", adds slots"
              d (% a (x 1) (y 2) (z 3)))
 (test/equal "\"d\" slot \"x\" is 1"  (? d x) 1)
 (test/equal "\"d\" slot \"y\" is 2"  (? d y) 2)
 (test/equal "\"d\" slot \"z\" is 3"  (? d z) 3)

 ;; (test/equal
 ;;  "Copying object-raw-clone functions"
 ;;  (let* ((o (% (% (% (current-root-object)
 ;;                     (a 1) (b 2) (c 3))
 ;;                  (b 4) (a 5) (d 6))
 ;;               (e 7) (b 8) (c 9))))
 ;;    (list
 ;;     (%protobj:slots (object-raw-clone/copy-immed-slots o))
 ;;     (%protobj:slots (object-raw-clone/copy-all-slots   o))))
 ;;  `(((c . 9) (b . 8) (e . 7))
 ;;    ((clone . ,object-raw-clone/no-slots-copy)
 ;;     (a . 5) (d . 6) (e . 7) (b . 8) (c . 9))))

 ;; TODO: Add more tests.
 )
