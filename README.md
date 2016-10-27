Context managers for SWI Prolog
===============================

Synopsis 
--------

```prolog
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



% Automatically revert changes to environment
?- with(setenv('FOO', 'BAR'), 
       (getenv('FOO', V),
        writeln(V))),
   getenv('FOO', V),
   writeln(V).
BAR
false.

% And you can define your own!
?- [user].
with:manage_context(my_ctx(X), assert(foo(X)), retract(foo(X))).
|: true.

?- with(my_ctx(1), listing(foo)), listing(foo). 
:- dynamic foo/1.

foo(1).

:- dynamic foo/1.

true
```

Description
-----------

This module provides context management for various types of Prolog
objects. Builtin support includes IO Streams, environment variables,
dynamic clauses, prolog flags, library(setting), and
library(debug). User defined contexts can be implemented using the
multifile predicate manage_context/3.  manage_context(Term, Setup,
Cleanup) defines a setup/cleanup pair for a specific type of
term. ```with(Term, Goal)``` calls =Goal= using setup_call_cleanup/3,
with the corresponding setup and cleanup goals. If `List` is a list of
terms then, ```with(List, Goal)``` nests the corresponding context
managers.

There is one difference between the semantics of setup_call_cleanup/3
and the corresponding goal using this library. =Setup= and =Cleanup=
goals must succeed. If they fail, the error
```error(mode_error(must_succeed, FailingGoal))``` is thrown.

For example, the provided manage_context/3 clause
for opening files could be defined:

```prolog
with:manage_context(open(File, Mode, Stream), 
                    open(File, Mode, Stream), 
                    close(Stream)).

```

The result is that the following are equivalent: 

```prolog
?- setup_call_cleanup(open(File, read, Stream), 
   is_stream(Stream), 
   close(Stream)). 

?- use_module(library(with)), 
   with(open(File, read, Stream), 
        is_stream(Stream)).
```

To show defined context managers, using ```listing/1```. E.g, the
context managers packaged with this module are:

```prolog
?- use_module(library(with)), listing(with:manage_context/3).

manage_context(open(A, C, D),  (absolute_file_name(A, B), open(B, C, D)), close(D)).
manage_context(assertz(A), assertz(A, B), erase(B)).
manage_context(setting(A, B),  (setting(A, C), set_setting(A, B)), set_setting(A, C)).

```

Installation
------------

Using SWI-Prolog 6.3 or later:

```prolog
?- pack_install(with).

```


Author: Eyal Dechter 
