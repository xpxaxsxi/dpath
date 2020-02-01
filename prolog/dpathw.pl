:- module(dpathw,[wexplore/1,
               wopen/1,
               file/1,
               dir/1,
               pathterm_atom/2,
               op(650,yfx, (:/))

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


%this is for convenience so that
% wexplore(dpath:filetype(X))
%works, instead of removing the dpath as in
% wexplore(filetype(X))
%
wexplore(dpath:T):-
          wexplore(T).


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
          dir(Drive:/T),
          pathterm_atom(Drive:/T,D2),
          show(dir,D2).



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

%this is for convenience so that
% wopen(dpath:filetype(X))
%works, instead of removing the dpath as in
% wopen(filetype(X))
%
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















