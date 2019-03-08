# dpath 
(this was: dirtree (temporary name, will be changed because)

An file system traversing utility. Backtracks in the directory structure. Got inspiration from xpath/2.

This project is under a MIT-license. 
I am currently trying to do a dpath-pack that can be added to swipl with pack_install/1 

``` prolog
?- directory_files('.',List).
List=['file1.txt','file2.txt','file3.pl'].
```

``` prolog
?- exists_file('file1.txt'). %old way
true.
```

``` prolog
?- file('file1.txt'). %the new way
true.
```

``` prolog
?- file(A).
A='file1.txt';
A='file2.txt';
A='file3.pl'
```

``` prolog
?- dpath:filetype(F.pl). %Swipl doesn't like  the dot in 'F.pl' and the Do What I Mean 
                   % proposes a proper module in Windows. Linux throws a error
F=file3
```

Linux example:
``` prolog
?- dir('/'/A/B/C/D/E/F).
A = usr,
B = include,
C = 'c++',
D = '4.7',
E = ext,
F = pb_ds
```

``` prolog
?- file('/'/A/B/'smb.conf').
A = etc,
B = samba 
```

`win` and `wexplorer` are modules that interact with Windows File Explorer. 
`dpath` can be used without them, as in Linux command line.

Example: 

Windows 10 opens up an Explorer window that has dpath.pl selected, when current
directory has a dpath.pl
```
?- win:wexplore(filetype(A.pl)).
A = dpath .
``` 

Example:

Windows 10 opens an jpg-image.  Next image is shown after user hits spacebar in Swi-Prolog command prompt. 
All desktops are traversed while searching for images. 
```
?- win:wopen(filetype('c:'/users/_/desktop/A.jpg)).
```

Possible bugs: Using Swi-Prolog dicts might cause problems.
