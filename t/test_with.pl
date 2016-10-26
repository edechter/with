

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(here, Dir)).

%% Project Imports
%% ===============
:- use_module(here('../prolog/with')).

%% Library Imports
%% ===============
:- use_module(library(settings)).
:- use_module(library(func)).


% a setting to test with
:- setting(my_mod:foo, boolean, true, 'a setting').


% Intercept debug_no_topic warning message, so it doesn't appear in test
% output.
:- multifile user:message_hook/3.
user:message_hook(debug_no_topic(_), _, _) :- !. 


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     TAP Tests
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
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
         
         
'with setenv no previous value' :-
    with(setenv('__not_an_env_var__', 'some value'),
         getenv('__not_an_env_var__', 'some value')),
    \+ getenv('__not_an_env_var__', _).


'with setenv restores previous value' :-
    with(setenv('FOO', 'BAR'),
         (
             with(setenv('FOO', 'BAZ'),
                  getenv('FOO', 'BAZ')),
             getenv('FOO', 'BAR')
         )).

'with set_prolog_flag throws error if flag does not exist'(throws(error(existence_error(prolog_flag, _), _))) :-
    with(set_prolog_flag('__this_flag_does_not_exist', true),
         current_prolog_flag('__this_flag_does_not_exist', _)). 


'with set_prolog_flag restores previous value' :-
    create_prolog_flag(my_flag, true, []), 
    with(set_prolog_flag(my_flag, false),
         current_prolog_flag(my_flag, false)),
    current_prolog_flag(my_flag, true).

'with debug restores to false if undefined' :-
    with(debug(my_debug),
         debugging(my_debug, true)),
    debugging(my_debug, false).

'with debug restores to false' :-
    nodebug(my_debug), 
    with(debug(my_debug),
         debugging(my_debug, true)),
    debugging(my_debug, false).

'with debug restores to true' :-
    debug(my_debug), 
    with(nodebug(my_debug),
         debugging(my_debug, false)),
    debugging(my_debug, true).

'with debug restores to redirection' :-
    tmp_file_stream(text, File, St), close(St),
    debug(my_debug), 
    debug(my_debug > 'foo.txt'),
    debug(my_debug > 'bar.txt'),
    sort(prolog_debug:debugging(my_debug, true, ~), OutList0), 
    with(nodebug(my_debug > 'foo.txt'),
         (debugging(my_debug, true),
          prolog_debug:debugging(my_debug, true, OutList1),
          \+ memberchk('foo.txt', OutList1)
         )
        ),
    sort(prolog_debug:debugging(my_debug, true, ~), OutList),
    OutList0 == OutList. 
    


'nested with' :-
    with([setenv('FOO', 'BAR'),
          assertz(foo(1)),
          assertz(bar(X) :- foo(X))],
         (
             getenv('FOO', 'BAR'),
             foo(1),
             bar(1)
         )
        ),
    \+ getenv('FOO', _),
    \+ foo(_),
    \+ bar(_).
    
