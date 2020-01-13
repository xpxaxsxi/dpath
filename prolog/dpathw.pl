:- module(dpathw,[wexplore/1,
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
:- use_module(wexplorer).


% Dict-concept is using the dot-operator
:- redefine_system_predicate( win:(.(_,_,_))).
.(Data, Func, Value):-  Value =.. ['.', Data,Func].






wexplore(dir(D)):-
          \+compound(D),
          dir(D),
          pathterm_atom(D,Dir),
          show(dir,Dir).

wexplore(dir(D/E)):-
          dir(D/E),
          pathterm_atom(D,Dir),
          show(dir,Dir).

wexplore(dir(Drive:/T)):-
          atom_concat(Drive,':',D2),
          wexplore(dir(D2/T)).


wexplore(file(D)):-
          \+compound(D),
          file(D),
          pathterm_atom(D,File),
          select(file,File).

wexplore(file(D/E)):-
          file(D/E),
          pathterm_atom(D/E,File),
          select(file,File).

wexplore(file(Drive:/T)):-
          file(Drive:/T),
          pathterm_atom(Drive:/T,File),
          select(file,File).

wexplore(filetype(Drive:/T)):-
          filetype(Drive:/T),
          pathterm_atom(Drive:/T,File),
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

wopen(filetype(Dr:/K/L)):-
          filetype(Dr:/K/L),
          pathterm_atom(Dr:/K/L,Path),
          absolute_file_name(Path,AbsPath),
          win_shell(open,AbsPath).

wopen(filetype(Dr:/K.L)):-
          filetype(Dr:/K.L),
          pathterm_atom(Dr:/K.L,Path),
          absolute_file_name(Path,AbsPath),
          win_shell(open,AbsPath).

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















