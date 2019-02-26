# dirtree (temporary name, will be changed)
An file system traversing utility. Backtracks in the directory structure. Got inspiration from xpath/2.

```
?- directory_files('.',List).
List=[file1.txt,file2.txt,file3.pl].
```

```
?- exists_file('file1.txt'). %old way
true.
```

```
?- file('file1.txt'). %the new way
true.
```

```
?- file(A).
A='file1.txt';
A='file2.txt';
A='file3.pl'
```

```
?- filetype(F.pl). %Swipl doesn't like  the dot in 'F.pl' and the Do What I Mean 
                   % proposes a proper module in Windows. Linux throws a error
F=file3
```

Linux example:
```
?- dir('/'/A/B/C/D/E/F).
A = usr,
B = include,
C = 'c++',
D = '4.7',
E = ext,
F = pb_ds
```

```
?- file('/'/A/B/'smb.conf').
A = etc,
B = samba 
```
Possible bugs: Using Swi-Prolog dicts might cause problems.
