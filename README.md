# dirtree (temporary name)
An file system traversing utility. Backtracks in the directory structure. Got inspiration from xpath/2.

?- directory_files('.',List).

List=\[file1.txt,file2.txt,file3.pl\].

?- exists_file('file1.txt'). %old way

true.

?- file('file1.txt'). %the new way

true.

?- file(A).

A='file1.txt';

A='file2.txt';

A='file3.pl'



?- filetype(F.pl).

F=file3



Possible bugs: Using Swi-Prolog dicts might cause problems.
