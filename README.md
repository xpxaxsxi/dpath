# dpath 

An file system traversing utility. Backtracks in the directory structure. Got inspiration from xpath/2.

This repository is under a MIT-license. 
This repository can be installed to Swi-Prolog as a pack by `pack_install(dpath).` 

```prolog
?- file(c:/ File). 
File = bootmgr ;
File = 'BOOTNXT' ;
File = 'bootTel.dat' .
```

```prolog
?- file('file1.txt'). 
true.
```

```prolog
?- file(A).
A='file1.txt';
A='file2.txt';
A='file3.pl'
```

```prolog
?- dpath:filetype(F.pl). %Swipl doesn't like  the dot in 'F.pl' and the Do What I Mean 
                  
F=file3
```

Linux example:
```prolog
?- dir('/'/A/B/C/D/E/F).
A = usr,
B = include,
C = 'c++',
D = '4.7',
E = ext,
F = pb_ds
```

```prolog
?- file('/'/A/B/'smb.conf').
A = etc,
B = samba 
```

Notes:
`dpathw` and `wexplorer` are modules that interact with Windows or Linux system utilities, that is,
they can open files in a proper program using the extension. Also it is possible to open a file system browser where file is selected, both in Linux and Windows. 

Filenames are not  produced in alphabetical order from file/1 or filetype/2, the file system decides the order, usually the order of files is same. In other words, you can't trust that filenames are produced in same order always. 

Filenames and extensions are case sensitive.



Example:

Aggregated size of prolog files in current directory. Windows and Linux.

```
?- dpath:(A.pl=X),aggregate_all(sum(Sz),(dpath:(filetype(X),pathterm_atom(X,Path)),size_file(Path,Sz)),Res).
X = A.pl,
Res = 1349131.
```

Example

Number of sub-directories in current directory. Windows and Linux.
```
?- aggregate_all(count,dpath:dir(X),Res).
Res = 66.
```

Example: 

Windows 10 opens up an Explorer window that has dpath.pl selected, when current
directory has only one file: the dpath.pl file. If current directory has more prolog files then 
for each file a new Explorer windows is opened.
```
?- dpathw:wexplore(filetype(A.pl)).
A = dpath .
``` 

Example:

Windows 10 opens an jpg-image.  Next image is shown after user hits spacebar in Swi-Prolog command prompt. 
All desktops are traversed while searching for images. 
```
?- dpathw:wopen(filetype(c:/users/_/desktop/A.jpg)).
```

Example:

Windows 10 shows a slideshow of jpg-images from desktop-folder, every users desktop is searched
```
?- dpathw:wopen(filetype(c:/users/_/desktop/A.jpg)), sleep(5), fail;!.
```

Possible bugs: Using Swi-Prolog dicts might cause problems.
