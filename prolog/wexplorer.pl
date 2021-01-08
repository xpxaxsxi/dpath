:- module(wexplorer,[show/2,select/2]).

show(dir,DirText):-
    atom(DirText),
    text_has_path(DirText,DirPath),
    exists_directory(DirPath),
    !,
    prolog_to_os_filename(DirPath,OS),
    atomic_list_concat(['explorer','/e',',',OS],' ',Comm),
    win_exec(Comm,show).

show(dir,SpecDir):-
    absolute_file_name(SpecDir,DirText),
    text_has_path(DirText,DirPath),
    exists_directory(DirPath),
    !,

    prolog_to_os_filename(DirPath,OS),atomic_list_concat(['explorer','/e',',',OS],' ',Comm),
    win_exec(Comm,show).

select(file,FileText):-
    atom(FileText),
    !,
    text_has_path(FileText,FilePath),
    exists_file(FilePath),

    prolog_to_os_filename(FilePath,OS),atomic_list_concat(['explorer','/select',',',OS],' ',Comm),
    win_exec(Comm,show).

select(file,SpecDir):-
    absolute_file_name(SpecDir,FileText),
    text_has_path(FileText,FilePath),
    exists_file(FilePath),

    prolog_to_os_filename(FilePath,OS),atomic_list_concat(['explorer','/select',',',OS],' ',Comm),
    win_exec(Comm,show).

%Directory path utility predicate
%A is a directory path that ends with `/'
%B is a directory path without ending `/'
text_has_path(A,B):-
    atom_concat(A,'/',B),!.

text_has_path(A,B):-
    atomic_concat(A,'/',B).
