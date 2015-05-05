#lang racket/base
;;; @Package     Protobj
;;; @Subtitle    Prototype-Delegation Object Model in Scheme
;;; @HomePage    http://www.neilvandyke.org/protobj/
;;; @Author      Neil Van Dyke
;;; @Version     0.4
;;; @Date        2011-11-08
;;; @PLaneT      neil/protobj:1:2

;; $Id: protobj.rkt,v 1.70 2011/03/04 07:52:42 neilpair Exp $

;;; @legal
;;; Copyright @copyright{} 2005--2011 Neil Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 3 of the License (LGPL 3), or (at your option)
;;; any later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/licenses/} for details.  For other licenses
;;; and consulting, please contact the author.
;;; @end legal

(require scheme/mpair
         srfi/9)

;;; @section Introduction

;;; Protobj is a Scheme library that implements a simple prototype-delegation
;;; object model, somewhat similar to that of
;;; @uref{http://research.sun.com/self/papers/self-power.html, Self}, and also
;;; related to those of SLIB @code{object} and OScheme.  Protobj was written
;;; mainly as a @code{syntax-rules} learning exercise, but also because people
;;; ask about prototype object models for Scheme from time to time.  Like most
;;; object systems, Protobj should be regarded as an amusement.  The Protobj
;;; library defines both a verbose set of procedures, and terse special syntax.
;;;
;;; Protobj is based on objects with named slots that can contain arbitrary
;;; values.  Object have immediate slots, and single parent objects from which
;;; additional slots are inherited.  When setting in a child object a slot
;;; inherited from the parent, a new immediate slot is created in the child so
;;; that the parent is unaffected and the slot is no longer inherited.
;;;
;;; Methods are simply closures stored in slots.  When a method is applied, the
;;; first term of the closure is the receiver object.  Unlike Self, getting the
;;; contents of the slot is distinguished from invoking a method contained in
;;; the slot.  This distinction was made due to the way first-class closures
;;; are often used in Scheme.
;;;
;;; An object is cloned by invoking the @code{clone} method.  The default root
;;; object's @code{clone} method creates a new child object without any
;;; immediate slots, rather than copying any slots.  This behavior can be
;;; overridden to always copy certain slots, to copy immediate slots, or to
;;; copy all inherited slots.  An overriding @code{clone} method can be
;;; implemented to apply its parent's @code{clone} method to itself and then
;;; set certain slots in the new child appropriately.
;;;
;;; Protobj requires R5RS, SRFI-9, SRFI-23, and SRFI-39.

;;; @section Tour

;;; The following is a quick tour of Protobj using the terse special syntax.
;;;
;;; @itemize
;;;
;;; @item
;;; Bind @code{a} to the new object that is created by cloning the default root
;;; object (@code{%} is special syntax for invoking the @code{clone} method):
;;; @lisp
;;; (define a (%))
;;; @end lisp
;;;
;;; @item
;;; Verify that @code{a} is an object and that @code{a}'s parent is the default
;;; root object:
;;; @lisp
;;; (object? a) @result{} #t
;;; (eq? (^ a) (current-root-object)) @result{} #t
;;; @end lisp
;;;
;;; @item
;;; Add to @code{a} a slot named @code{x} with value @code{1}:
;;; @lisp
;;; (! a x 1)
;;; @end lisp
;;;
;;; @item
;;; Get @code{a}'s slot @code{x}'s value:
;;; @lisp
;;; (? a x) @result{} 1
;;; @end lisp
;;;
;;; @item
;;; Bind @code{b} to a clone of @code{a}:
;;; @lisp
;;; (define b (% a))
;;; @end lisp
;;;
;;; @item
;;; Get @code{b}'s slot @code{x}'s value, which is inherited from @code{a}:
;;; @lisp
;;; (? b x) @result{} 1
;;; @end lisp
;;;
;;; @item
;;; Set @code{a}'s slot @code{x}'s value to @code{42}, and observe that
;;; @code{b} inherits the new value:
;;; @lisp
;;; (! a x 42)
;;; (? a x) @result{} 42
;;; (? b x) @result{} 42
;;; @end lisp
;;;
;;; @item
;;; Set @code{b}'s slot @code{x}'s value to @code{69}, and observe that @var{a}
;;; retains its own @code{x} value although @var{b}'s @code{x} value has been
;;; changed:
;;; @lisp
;;; (! b x 69)
;;; (? a x) @result{} 42
;;; (? b x) @result{} 69
;;; @end lisp
;;;
;;; @item
;;; Add to @code{a} an @code{xplus} slot containing a closure that implements a
;;; method of the object:
;;; @lisp
;;; (! a xplus (lambda ($ n) (+ (? $ x) n)))
;;; @end lisp
;;;
;;; @item
;;; Apply the method to the @code{a} and @code{b} objects (@code{b} inherits
;;; any new slots added to @code{a}):
;;; @lisp
;;; (@@ a xplus 7) @result{} 49
;;; (@@ b xplus 7) @result{} 76
;;; @end lisp
;;;
;;; @item
;;; Observe the shorthand syntax for applying methods to an object multiple
;;; times, with the syntax having the value of the lastmost application:
;;; @lisp
;;; (@@ a (xplus 1000) (xplus 7)) @result{} 49
;;; @end lisp
;;;
;;; @item
;;; Bind to @var{c} an object that clones @var{a} and adds slot @var{y} with
;;; value @code{101}:
;;; @lisp
;;; (define c (% a (y 101)))
;;; @end lisp
;;;
;;; @item
;;; Get the values of both the @code{x} and @code{y} slots of @code{c}:
;;; @lisp
;;; (? c x y) @result{} 42 101
;;; @end lisp
;;;
;;; @item
;;; Finally, bind @code{d} to a clone of @code{a} that overrides @code{a}'s
;;; @code{x} slot:
;;; @lisp
;;; (define d (% a (x 1) (y 2) (z 3)))
;;; (? d x y z) @result{} 1 2 3
;;; @end lisp
;;;
;;; @end itemize

;;; @section Basic Interface

;;; The basic interface of Protobj is a set of procedures.

(define-record-type object
  (%protobj:make-object parent slots)
  object?
  (parent object-parent          %protobj:set-parent!)
  (slots  %protobj:slots %protobj:set-slots!))

(define (%protobj:slots-assq slot-symbol slots)
  ;; Note: Sadly, we are no longer using assq for lookup, so it's slower.
  (let loop ((slots slots))
    (cond ((null? slots)                        #f)
          ((eq? slot-symbol (mcar (car slots))) (car slots))
          (else                                 (loop (cdr slots))))))

(define (%protobj:find-slot obj slot-symbol proc noslot-thunk)
  (let loop ((o obj))
    (cond ((%protobj:slots-assq slot-symbol (%protobj:slots o)) => proc)
          (else (cond ((object-parent o) => loop)
                      (else (noslot-thunk)))))))

;;; @defproc object? x
;;;
;;; Predicate for whether or not @var{x} is a Protobj object.

;; see define-record-type

;;; @defproc object-parent obj
;;;
;;; Yields the parent object of object @var{obj}.

;; see define-record-type

;; TODO: Expose a "set-object-parent!"?

;;; @defproc object-set! obj slot-symbol val
;;;
;;; Sets the slot identified by symbol @var{slot-symbol} in object @var{obj} to
;;; value @code{val}.

(define (object-set! obj slot-symbol val)
  (let ((slots (%protobj:slots obj)))
    (cond ((%protobj:slots-assq slot-symbol slots)
           => (lambda (slot) (set-mcdr! slot val)))
          (else (%protobj:set-slots! obj (cons (mcons slot-symbol val)
                                               slots))))))

;;; @defproc object-get obj slot-symbol
;;;
;;; Yields the value of slot named by symbol @var{slot-symbol} in object
;;; @var{obj} (immediate or inherited).  If no slot of that name exists, an
;;; error is signaled.

(define (object-get obj slot-symbol)
  (%protobj:find-slot
   obj
   slot-symbol
   mcdr
   (lambda () (error "Object has no such slot:" obj slot-symbol))))

;; (define (object-get/procs obj slot-symbol proc noslot-thunk)
;;   (%protobj:find-slot obj
;;                               slot-symbol
;;                               (lambda (slot) (proc (cdr slot)))
;;                               noslot-thunk))

;;; @defproc object-get obj slot-symbol noslot-thunk
;;;
;;; Yields the value of slot named by symbol @var{slot-symbol} in object
;;; @var{obj} (immediate or inherited), if any such slot exists.  If no slot of
;;; that name exists, then yields the value of applying closure
;;; @var{noslot-thunk}.

(define (object-get/noslot-thunk obj slot-symbol noslot-thunk)
  (%protobj:find-slot obj
                      slot-symbol
                      mcdr
                      noslot-thunk))

;;; @defproc object-apply obj slot-symbol arg ...
;;;
;;; Applies the method (closure) in the slot named by @var{slot-symbol} of
;;; object @var{obj}.  The first term of the method is @var{obj}, and one or
;;; more @var{arg} are the remaining terms.  If no such slot exists, an error
;;; is signaled.

(define (object-apply obj slot-symbol . args)
  (apply (object-get obj slot-symbol) obj args))

;;; @defproc object-apply/noslot-thunk obj noslot-thunk slot-symbol arg ...
;;;
;;; Like @code{object-apply}, except that, if the slot does not exist, instead
;;; of signalling an error, the value is the result of applying
;;; @var{noslot-thunk}.

(define (object-apply/noslot-thunk obj slot-symbol noslot-thunk . args)
  (%protobj:find-slot obj
                      slot-symbol
                      (lambda (slot) (apply (mcdr slot) obj args))
                      noslot-thunk))

;; TODO: Implement "object-apply/try", which calls a thunk (or is a no-op) if
;; no slot can be found.  Maybe special syntax for doing this apply/try to a
;; parent.  One of the things this might be most useful for is in a "clone"
;; method, to invoke any parent "clone" method within additional behavior.

;;; @defproc  object-raw-clone/no-slots-copy    obj
;;; @defprocx object-raw-clone/copy-immed-slots obj
;;; @defprocx object-raw-clone/copy-all-slots   obj
;;;
;;; These procedures implement different ways of cloning an object, and are
;;; generally bound as @code{clone} methods in root objects.
;;; @code{/no-slots-copy} does not copy any slots, @code{/copy-immed-slots}
;;; copes immediate slots, and @code{/copy-all-slots} copies all slots
;;; including inherited ones.

(define (object-raw-clone/no-slots-copy obj)
  (%protobj:make-object obj '()))

(define (object-raw-clone/copy-immed-slots obj)
  (%protobj:make-object obj
                        (map (lambda (pair)
                               (mcons (mcar pair) (mcdr pair)))
                             (%protobj:slots obj))))

(define (object-raw-clone/copy-all-slots obj)
  ;; Note: We could save a few "(%protobj:slots-assq X '())" calls by copying
  ;; the immediate slots first.
  (let loop-objs ((o    obj)
                  (seen '()))
    (if o
        (let loop-slots ((slots  (%protobj:slots o))
                         (result seen))
          (if (null? slots)
              (loop-objs (object-parent o) result)
              (loop-slots (cdr slots)
                          (let ((name (mcar (car slots))))
                            (if (%protobj:slots-assq name seen)
                                result
                                (cons (mcons name (mcdr (car slots)))
                                      result))))))
        (%protobj:make-object obj seen))))

;; (define (object-clone obj)
;;   (object-apply obj 'clone))

;;; @defparam current-root-object
;;;
;;; Parameter for the default root object.  The initial value is a root object
;;; that has @code{object-raw-clone/no-slots-copy} in its @code{clone} slot.

;; TODO: Make this a parameter, or lose it altogether.

(define current-root-object
  (make-parameter
   (%protobj:make-object
    #f
    (list (mcons 'clone object-raw-clone/no-slots-copy)))))

;;; @section Terse Syntax

;;; Since Protobj's raison d'etre was to play with syntax, here it is.  Note
;;; that slot names are never quoted.

;;; @defsyntax ^ obj
;;;
;;; Parent of @var{obj}.

(define-syntax ^ (syntax-rules () ((_ OBJ) (object-parent OBJ))))

;;; @defsyntax  ! obj slot val
;;; @defsyntaxx ! obj (slot val) ...
;;;
;;; Sets object @var{obj}'s slot @var{slot}'s value to @var{val}.  In the
;;; second form of this syntax, multiple slots of @var{obj} may be set at once,
;;; and are set in the order given.

(define-syntax !
  (syntax-rules ()
    ((_ OBJ (S0 V0) (S1 V1) ...) (let ((temp OBJ))
                                   (! temp S0 V0)
                                   (! temp S1 V1) ...))
    ((_ OBJ S V)                   (object-set! OBJ (quote S) V))))

;;; @defsyntax ? obj slot ...
;;;
;;; Yields the values of the given @var{slot}s of @var{obj}.  If more than one
;;; @var{slot} is given, a multiple-value return is used.

(define-syntax ?
  (syntax-rules ()
    ((_ OBJ S)      (object-get OBJ (quote S)))
    ((_ OBJ S0 ...) (let ((temp OBJ)) (values (? temp S0) ...)))))

;;; @defsyntax  @@ obj slot arg ...
;;; @defsyntaxx @@ obj (slot arg ... ) ...
;;;
;;; Applies @var{obj}'s @var{slot} method, with @var{obj} as the first term and
;;; @var{arg}s as the remaining terms.  In the second form of this syntax,
;;; multiple methods may be applied, and the value is the value of the last
;;; method application.

(define-syntax %protobj:apply*
  (syntax-rules ()
    ((_ (X0 X1 ...) S A0 ...) (let ((temp (X0 X1 ...)))
                                (%protobj:apply* temp S A0 ...)))
    ((_ OVAR        S A0 ...) ((object-get OVAR (quote S)) OVAR A0 ...))))

(define-syntax @
  (syntax-rules ()
    ((_ OBJ (S0 A0 ...) (S1 A1 ...) ...)
     (let ((temp OBJ))
       (%protobj:apply* temp S0 A0 ...)
       (%protobj:apply* temp S1 A1 ...) ...))
    ((_ OBJ S A ...)
     (%protobj:apply* OBJ S A ...))))

;;; @defsyntax % [ obj (slot val) ... ]
;;;
;;; Clones object @var{obj}, binding any given @var{slot}s to respective given
;;; @var{val}s.

(define-syntax %
  (syntax-rules ()
    ((_)                         (% (current-root-object)))
    ((_ OBJ)                     (@ OBJ clone))
    ((_ OBJ (S0 V0) (S1 V1) ...) (let ((temp (% OBJ)))
                                   (! temp S0 V0)
                                   (! temp S1 V1) ...
                                   temp))))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.4 --- 2011-11-08 --- PLaneT @code{(1 2)}
;;; Fixed @code{object?} not being exported.  (Thanks to Shviller for reporting.)
;;;
;;; @item Version 0.3 --- 2009-03-03 --- PLaneT @code{(1 1)}
;;; License is now LGPL 3.  Converted to authors new Scheme administration
;;; system.  Changed slot lists and slot pairs to be explicitly mutable, for
;;; PLT 4.x.
;;;
;;; @item Version 0.2 --- 2005-06-19 -- PLaneT @code{(1 0)}
;;; Fixed bug in @code{%protobj:apply*} (thanks to Benedikt Rosenau for
;;; reporting).  Changed @code{$} syntax to @code{?}, so that @code{$} could be
;;; used for ``self'' in methods.  Documentation changes.
;;;
;;; @item Version 0.1 --- 2005-01-05
;;; Initial release.
;;;
;;; @end table

(provide
 !
 %
 ?
 @
 ^
 current-root-object
 object?
 object-apply
 object-apply/noslot-thunk
 object-get
 object-get/noslot-thunk
 object-raw-clone/copy-all-slots
 object-raw-clone/copy-immed-slots
 object-raw-clone/no-slots-copy
 object-set!)
