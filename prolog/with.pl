

:- module(with, [
              with/2,           % +Term, :Goal
              manage_context/3 % +Term, :Setup, :Cleanup
          ]).

/** <module> Context managers for SWI Prolog

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

@author Eyal Dechter <eyaldechter@gmail.com>

*/


%!    with(+Term, :Goal) is det
%
%     Call =Goal= with the context manager associated with =Term=. 
%
%     @throws error(instantiation_error, _)          If =Term= is a variable.
%     @throws error(mode_error(must_succeed, Goal))  If =Goal= is a setup or cleanup goal for context and =Goal= does not succeed.
%     

:- meta_predicate with(:, 0).
with(MTerm, Goal) :- 
    strip_module(MTerm, M, Term),
    (term_has_context_manager(Term, M, Setup, Cleanup) ->
         setup_call_cleanup(ctx_must_succeed(M:Setup),
                            Goal,
                            ctx_must_succeed(M:Cleanup))
    ;
    existence_error(context_manager, MTerm)
    ).


ctx_must_succeed(M:Goal) :-
    (M:Goal -> true
    ;
    throw(error(mode_error(must_succeed, M:Goal), _))
    ).


%!    term_has_context_manager(+Term, +Module, -Setup: callable, -Cleanup: cleanup) is semidet
%
%     True if there is a unique context manager for Term. 
term_has_context_manager(Term, M, M:Setup, M:Cleanup) :-
    must_be(nonvar, Term),
    manage_context(Term, Setup, Cleanup).



%!    manage_context(Term, :Setup, :Cleanup) is det
%
%     If true, goals =Setup= and =Cleanup= are called to manage
%     context associated with =Term=. Use this multifile predicate to
%     define context managers.
:- multifile manage_context/3.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     IO Stream context managers
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
manage_context(open(File, Mode, Stream),
               (
                   absolute_file_name(File, AbsFile), 
                   open(AbsFile, Mode, Stream)
               ),
               close(Stream)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Dynamic DB context managers
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
manage_context(assertz(Clause), assertz(Clause, Ref), erase(Ref)).
manage_context(asserta(Clause), asserta(Clause, Ref), erase(Ref)).



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     library(settings) context managers
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
manage_context(setting(Setting, NewValue),
               (
                   setting(Setting, OldValue),
                   set_setting(Setting, NewValue)
               ),
               set_setting(Setting, OldValue)
              ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     library(debug) context managers
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
manage_context(debug(Spec),
               (
                   prolog_debug:debug_target(Spec, Topic, _),
                   (debugging(Topic, Previous) -> true; Previous = false),
                   debug(Spec)
               ),
               (
                   nodebug(Spec),
                   ( (Previous == true, debugging(Topic, false)) ->
                          debug(Topic)
                     ;
                     true
                   )
               )
              ).

manage_context(nodebug(Spec),
               nodebug(Spec),
               debug(Spec)). 

                     
                

                    

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     setenv/unsetenv context manager 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
manage_context(setenv(Name, Value),
               (
                   (getenv(Name, V) ->
                        Previous = just(V)                        
                   ;
                   Previous = nothing                   
                   ),
                  setenv(Name, Value) 
               ),
               (Previous = just(V) ->
                    setenv(Name, V)
               ;
               unsetenv(Name)                   
               )
              ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     set_prolog_flag context manager
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
manage_context(set_prolog_flag(Name, NewValue),
               (
                   (current_prolog_flag(Name, OldValue) ->
                        set_prolog_flag(Name, NewValue)
                   ;
                   existence_error(prolog_flag, Name)
                   )
               ),
               set_prolog_flag(Name, OldValue)
              ).
               


                    
                   
                   
                        
