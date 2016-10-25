

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(here, Dir)).

:- use_module(here('../prolog/with')).


:- use_module(library(settings)).
:- setting(my_mod:foo, boolean, true, 'a setting').


:- use_module(library(tap)).

'with file' :-
    
    with(open(here('test_file.txt'), read, Stream),
         is_stream(Stream)
        ),
    \+ is_stream(Stream).

'with setting' :-
    with(setting(my_mod:foo, false),
         setting(my_mod:foo, false)),
    setting(my_mod:foo, true).

'with non-existent setting'(throws(error(existence_error(setting, my_mod:for), _))) :-
    with(setting(my_mod:for, false),
         setting(my_mod:foo, false)),
    setting(my_mod:foo, true).

'no context manager 1'(throws(error(existence_error(context_manager, user:foo), _))) :-
    with(foo, true).

'context spec is var'(throws(error(instantiation_error, _))) :- 
    with(_, true).

'non-det goal' :-
    findall(X, 
            with(setting(my_mod:foo, false), member(X, [1,2,3])),
            Xs),
    Xs == [1,2,3].

    
'asserted clause' :-
    findall(X, 
            with(assertz((foo(X) :- member(X, [1,2,3]))), foo(X)),
            Xs),
    Xs == [1,2,3],
    \+ clause(foo(X), _).

'with failing setup'(throws(error(mode_error(must_succeed, _), _))) :-
    with(        
        assertz(with:manage_context(foo, fail, true)),
        with(foo, true)
    ).
         
         
         
         

    
