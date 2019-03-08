:- module(win,[wexplore/1,
               wopen/1,
               file/1,
               dir/1,
               pathterm_atom/2
              ]).
/* Module has predicates to interact with Windows file tools
 *
 *
*/

:- use_module(dpath).
:- use_module(sources_root(wexplorer)).


% Dict-concept is using the dot-operator
:- redefine_system_predicate( win:(.(_,_,_))).
.(Data, Func, Value):-  Value =.. ['.', Data,Func].

wexplore(T):-
          compound(T),
          T=(dpath:T2),
          wexplore(T2).

wexplore(dir(D)):-
          dir(D),
          pathterm_atom(D,Dir),
          show(dir,Dir).

wexplore(file(D)):-
          file(D),
          pathterm_atom(D,File),
          select(file,File).

wexplore(filetype(K/L)):-
          filetype(K/L),
          pathterm_atom(K/L,Path),
          absolute_file_name(Path,AbsPath),
          select(file,AbsPath).

wexplore(filetype(K.L)):-
          filetype(K.L),
          pathterm_atom(K.L,Path),
          absolute_file_name(Path,AbsPath),
          select(file,AbsPath).

wopen(dpath:T):-
          wopen(T).

wopen(filetype(K/L)):-
          filetype(K/L),
          pathterm_atom(K/L,Path),
          absolute_file_name(Path,AbsPath),
          win_shell(open,AbsPath).

wopen(filetype(K.L)):-
          filetype(K.L),
          pathterm_atom(K.L,Path),
          absolute_file_name(Path,AbsPath),
          win_shell(open,AbsPath).















