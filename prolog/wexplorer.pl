:- module(wexplorer,[show/2,select/2,open/1]).

show(dir,DirText):-
    atom(DirText),
    text_has_path(DirText,DirPath),
    exists_directory(DirPath),
    !,
    cmd_open_file(DirPath).

show(dir,SpecDir):-
    absolute_file_name(SpecDir,DirText),
    text_has_path(DirText,DirPath),
    exists_directory(DirPath),
    !,
    cmd_open_file(DirPath).

select(file,SpecDir):-
    absolute_file_name(SpecDir,FileText),
    exists_file(FileText),

    cmd_select_file(FileText).

:- if(current_prolog_flag(unix,true)).
open(File):-
    atomic_list_concat(['xdg-open ','\"',File,'\"'],Atom),
    shell(Atom).

cmd_open_file(File):-
    current_prolog_flag(unix,true),!,
    atomic_list_concat(['xdg-open',File],' ',Atom),
    shell(Atom).

cmd_select_file(File):-
    current_prolog_flag(unix,true),!,
    unix_select_file(File).

unix_select_file(File):-
    absolute_file_name(File,AbsFile),
A='dbus-send --session --type=method_call    --dest="org.freedesktop.FileManager1"     "/org/freedesktop/FileManager1"     "org.freedesktop.FileManager1.ShowItems" array:string:"file:////',B=AbsFile,C='" string:""',

    atomic_list_concat([A,B,C],Comm),
    shell(Comm).


:- endif.

:- if(current_prolog_flag(windows,true)).

open(File):-
    win_shell(open,File).

cmd_open_file(FilePath):-
    prolog_to_os_filename(FilePath,OS),
    atomic_list_concat(['explorer','/e',',',OS],' ',Comm),
    win_exec(Comm,show).

cmd_select_file(FilePath):-
    current_prolog_flag(windows,true),!,
    prolog_to_os_filename(FilePath,OS),
    atomic_list_concat(['explorer','/select',',',OS],' ',Comm),
    win_exec(Comm,show).


:- endif.


text_has_path(A,B):-
    atom_concat(A,'/',B),!.

text_has_path(A,B):-
    atomic_concat(A,'/',B).















