#lang scribble/doc
@; THIS FILE IS GENERATED
@(require scribble/manual)
@(require (for-label (planet neil/protobj:1:2)))
@(require (for-label racket/base))
@title[#:version "0.4"]{@bold{Protobj}: Prototype-Delegation Object Model in Scheme}
@author{Neil Van Dyke}


License: @seclink["Legal" #:underline? #f]{LGPL 3} @(hspace 1) Web: @link["http://www.neilvandyke.org/protobj/" #:underline? #f]{http://www.neilvandyke.org/protobj/}

@defmodule[(planet neil/protobj:1:2)]

      

@section{Introduction}


      

Protobj is a Scheme library that implements a simple prototype-delegation object model, somewhat similar to that of @link["http://research.sun.com/self/papers/self-power.html"]{Self}, and also related to those of SLIB @tt{object} and OScheme.  Protobj was written mainly as a @tt{syntax-rules} learning exercise, but also because people ask about prototype object models for Scheme from time to time.  Like most object systems, Protobj should be regarded as an amusement.  The Protobj library defines both a verbose set of procedures, and terse special syntax.


      

Protobj is based on objects with named slots that can contain arbitrary values.  Object have immediate slots, and single parent objects from which additional slots are inherited.  When setting in a child object a slot inherited from the parent, a new immediate slot is created in the child so that the parent is unaffected and the slot is no longer inherited.


      

Methods are simply closures stored in slots.  When a method is applied, the first term of the closure is the receiver object.  Unlike Self, getting the contents of the slot is distinguished from invoking a method contained in the slot.  This distinction was made due to the way first-class closures are often used in Scheme.


      

An object is cloned by invoking the @tt{clone} method.  The default root object's @tt{clone} method creates a new child object without any immediate slots, rather than copying any slots.  This behavior can be overridden to always copy certain slots, to copy immediate slots, or to copy all inherited slots.  An overriding @tt{clone} method can be implemented to apply its parent's @tt{clone} method to itself and then set certain slots in the new child appropriately.


      

Protobj requires R5RS, SRFI-9, SRFI-23, and SRFI-39.


    
      

@section{Tour}


      

The following is a quick tour of Protobj using the terse special syntax.


      

@itemize[
@item{
          

Bind @tt{a} to the new object that is created by cloning the default root object (@tt{%} is special syntax for invoking the @tt{clone} method):


          

@SCHEMEBLOCK[
(define a (%))
]


        }
@item{
          

Verify that @tt{a} is an object and that @tt{a}'s parent is the default root object:


          

@SCHEMEBLOCK[
(object? a) ==> #t
(eq? (^ a) (current-root-object)) ==> #t
]


        }
@item{
          

Add to @tt{a} a slot named @tt{x} with value @tt{1}:


          

@SCHEMEBLOCK[
(! a x 1)
]


        }
@item{
          

Get @tt{a}'s slot @tt{x}'s value:


          

@SCHEMEBLOCK[
(? a x) ==> 1
]


        }
@item{
          

Bind @tt{b} to a clone of @tt{a}:


          

@SCHEMEBLOCK[
(define b (% a))
]


        }
@item{
          

Get @tt{b}'s slot @tt{x}'s value, which is inherited from @tt{a}:


          

@SCHEMEBLOCK[
(? b x) ==> 1
]


        }
@item{
          

Set @tt{a}'s slot @tt{x}'s value to @tt{42}, and observe that @tt{b} inherits the new value:


          

@SCHEMEBLOCK[
(! a x 42)
(? a x) ==> 42
(? b x) ==> 42
]


        }
@item{
          

Set @tt{b}'s slot @tt{x}'s value to @tt{69}, and observe that @schemevarfont{a} retains its own @tt{x} value although @schemevarfont{b}'s @tt{x} value has been changed:


          

@SCHEMEBLOCK[
(! b x 69)
(? a x) ==> 42
(? b x) ==> 69
]


        }
@item{
          

Add to @tt{a} an @tt{xplus} slot containing a closure that implements a method of the object:


          

@SCHEMEBLOCK[
(! a xplus (lambda ($ n) (+ (? $ x) n)))
]


        }
@item{
          

Apply the method to the @tt{a} and @tt{b} objects (@tt{b} inherits any new slots added to @tt{a}):


          

@SCHEMEBLOCK[
(\@ a xplus 7) ==> 49
(\@ b xplus 7) ==> 76
]


        }
@item{
          

Observe the shorthand syntax for applying methods to an object multiple times, with the syntax having the value of the lastmost application:


          

@SCHEMEBLOCK[
(\@ a (xplus 1000) (xplus 7)) ==> 49
]


        }
@item{
          

Bind to @schemevarfont{c} an object that clones @schemevarfont{a} and adds slot @schemevarfont{y} with value @tt{101}:


          

@SCHEMEBLOCK[
(define c (% a (y 101)))
]


        }
@item{
          

Get the values of both the @tt{x} and @tt{y} slots of @tt{c}:


          

@SCHEMEBLOCK[
(? c x y) ==> 42 101
]


        }
@item{
          

Finally, bind @tt{d} to a clone of @tt{a} that overrides @tt{a}'s @tt{x} slot:


          

@SCHEMEBLOCK[
(define d (% a (x 1) (y 2) (z 3)))
(? d x y z) ==> 1 2 3
]


        }


]


    
      

@section{Basic Interface}


      

The basic interface of Protobj is a set of procedures.


      

@defproc[ (object? (x any/c)) any/c]{
          

Predicate for whether or not @schemevarfont{x} is a Protobj object.


        }

      

@defproc[ (object-parent (obj any/c)) any/c]{
          

Yields the parent object of object @schemevarfont{obj}.


        }

      

@defproc[ (object-set! (obj any/c) (slot-symbol any/c) (val any/c)) any/c]{
          

Sets the slot identified by symbol @schemevarfont{slot-symbol} in object @schemevarfont{obj} to value @tt{val}.


        }

      

@defproc[ (object-get (obj any/c) (slot-symbol any/c)) any/c]{
          

Yields the value of slot named by symbol @schemevarfont{slot-symbol} in object @schemevarfont{obj} (immediate or inherited).  If no slot of that name exists, an error is signaled.


        }

      

@defproc[ (object-get (obj any/c) (slot-symbol any/c) (noslot-thunk any/c)) any/c]{
          

Yields the value of slot named by symbol @schemevarfont{slot-symbol} in object @schemevarfont{obj} (immediate or inherited), if any such slot exists.  If no slot of that name exists, then yields the value of applying closure @schemevarfont{noslot-thunk}.


        }

      

@defproc[ (object-apply (obj any/c) (slot-symbol any/c) (arg any/c) (... any/c)) any/c]{
          

Applies the method (closure) in the slot named by @schemevarfont{slot-symbol} of object @schemevarfont{obj}.  The first term of the method is @schemevarfont{obj}, and one or more @schemevarfont{arg} are the remaining terms.  If no such slot exists, an error is signaled.


        }

      

@defproc[ (object-apply/noslot-thunk (obj any/c) (noslot-thunk any/c) (slot-symbol any/c) (arg any/c) (... any/c)) any/c]{
          

Like @tt{object-apply}, except that, if the slot does not exist, instead of signalling an error, the value is the result of applying @schemevarfont{noslot-thunk}.


        }

      

@defproc[ (object-raw-clone/no-slots-copy (obj any/c)) any/c]{}


@defproc[ (object-raw-clone/copy-immed-slots (obj any/c)) any/c]{}


@defproc[ (object-raw-clone/copy-all-slots (obj any/c)) any/c]{
          

These procedures implement different ways of cloning an object, and are generally bound as @tt{clone} methods in root objects. @tt{/no-slots-copy} does not copy any slots, @tt{/copy-immed-slots} copes immediate slots, and @tt{/copy-all-slots} copies all slots including inherited ones.


        }

      

@defparam[current-root-object x any/c]{
          

Parameter for the default root object.  The initial value is a root object that has @tt{object-raw-clone/no-slots-copy} in its @tt{clone} slot.


        }

    
      

@section{Terse Syntax}


      

Since Protobj's raison d'etre was to play with syntax, here it is.  Note that slot names are never quoted.


      

@defform[#:id ^ (^ obj)]{
          

Parent of @schemevarfont{obj}.


        }

      

@defform[#:id ! (! obj slot val)]{}


@defform[#:id ! (! obj ( slot val ) ...)]{
          

Sets object @schemevarfont{obj}'s slot @schemevarfont{slot}'s value to @schemevarfont{val}.  In the second form of this syntax, multiple slots of @schemevarfont{obj} may be set at once, and are set in the order given.


        }

      

@defform[#:id ? (? obj slot ...)]{
          

Yields the values of the given @schemevarfont{slot}s of @schemevarfont{obj}.  If more than one @schemevarfont{slot} is given, a multiple-value return is used.


        }

      

@defform[#:id |@| (|@| obj slot arg ...)]{}


@defform[#:id |@| (|@| obj ( slot arg ... ) ...)]{
          

Applies @schemevarfont{obj}'s @schemevarfont{slot} method, with @schemevarfont{obj} as the first term and @schemevarfont{arg}s as the remaining terms.  In the second form of this syntax, multiple methods may be applied, and the value is the value of the last method application.


        }

      

@defform[#:id % (% obj ( slot val ) ...)]{
          

Clones object @schemevarfont{obj}, binding any given @schemevarfont{slot}s to respective given @schemevarfont{val}s.


        }

    
      

@section{History}


      

@itemize[

@item{Version 0.4 --- 2011-11-08 --- PLaneT @tt{(1 2)}
            

Fixed @tt{object?} not being exported.  (Thanks to Shviller for reporting.)


          }


@item{Version 0.3 --- 2009-03-03 --- PLaneT @tt{(1 1)}
            

License is now LGPL 3.  Converted to authors new Scheme administration system.  Changed slot lists and slot pairs to be explicitly mutable, for PLT 4.x.


          }


@item{Version 0.2 --- 2005-06-19 -- PLaneT @tt{(1 0)}
            

Fixed bug in @tt{%protobj:apply*} (thanks to Benedikt Rosenau for reporting).  Changed @tt{$} syntax to @tt{?}, so that @tt{$} could be used for ``self'' in methods.  Documentation changes.


          }


@item{Version 0.1 --- 2005-01-05
            

Initial release.


          }



]


    

@section[#:tag "Legal"]{Legal}



Copyright (c) 2005--2011 Neil Van Dyke.  This program is Free Software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3 of the License (LGPL 3), or (at your option) any later version.  This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.  See http://www.gnu.org/licenses/ for details.  For other licenses and consulting, please contact the author.



@italic{@smaller{Standard Documentation Format Note: The API
signatures in this documentation are likely incorrect in some regards, such as
indicating type @tt{any/c} for things that are not, and not indicating when
arguments are optional.  This is due to a transitioning from the Texinfo
documentation format to Scribble, which the author intends to finish
someday.}}
