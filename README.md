Context managers for SWI Prolog
===============================

Synopsis 
--------

```
?- use_module(library(with)). 

% IO streams automatically closed
?- with(open('/tmp/foo.txt', write, Stream), 
        format(Stream, 'My context was managed!', [])).


% Asserted clauses automatically retracted.
?- dynamic(foo/1),
   with(assertz(foo(1) :- true),
        (foo(X),
         writeln(X))),        
   foo(Y),
   writeln(Y).   
1
false.

% Changed setting automatically reverted
?- [user].
:- setting(mymod:foo, boolean, true, '').
|: true.
with(setting(mymod:foo, false), setting(mymod:foo, V)), setting(mymod:foo, V1). 
V = false,
V1 = true.

```

Description
-----------

This module provides context management for various types of Prolog
objects, such as IO streams, dynamic clauses, and settings.  User
defined contexts can be implemented using the multifile predicate
manage_context/3.  manage_context(Term, Setup, Cleanup) defines a
setup/cleanup pair for a specific type of term. ```with(Term, Goal)```
calls =Goal= using setup_call_cleanup/3, with the corresponding setup
and cleanup goals. 

There is one difference between the semantics of setup_call_cleanup/3
and the corresponding goal using this library. =Setup= and =Cleanup=
goals must succeed. If they fail, the error
```error(mode_error(must_succeed, FailingGoal))``` is thrown.

For example, the provided manage_context/3 clause
for opening files could be defined:

```
with:manage_context(open(File, Mode, Stream), 
                    open(File, Mode, Stream), 
                    close(Stream)).

```

The result is that the following are equivalent: 

```
?- setup_call_cleanup(open(File, read, Stream), 
   is_stream(Stream), 
   close(Stream)). 

?- use_module(library(with)), 
   with(open(File, read, Stream), 
        is_stream(Stream)).
```

To show defined context managers, using ```listing/1```. E.g, the
context managers packaged with this module are:

```
?- use_module(library(with)), listing(with:manage_context/3).

manage_context(open(A, C, D),  (absolute_file_name(A, B), open(B, C, D)), close(D)).
manage_context(assertz(A), assertz(A, B), erase(B)).
manage_context(setting(A, B),  (setting(A, C), set_setting(A, B)), set_setting(A, C)).

```

Installation
------------

Using SWI-Prolog 6.3 or later:

```
?- pack_install(with).

```


Author: Eyal Dechter 
