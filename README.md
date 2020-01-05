This is a charlie-branch, where I try to learn about git and about github

# dpath 

An file system traversing utility. Backtracks in the directory structure. Got inspiration from xpath/2.

This repository is under a MIT-license. 
This repository can be installed to Swi-Prolog as a pack by `pack_install(dpath).` 

```prolog
?- file(Drive :/ File'). 
Drive = c,
File = bootmgr ;
Drive = c,
File = 'BOOTNXT' ;
Drive = c,
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
                   % proposes a proper module in Windows. Linux throws a error
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

`dpathw` and `wexplorer` are modules that interact with Windows File Explorer. 
`dpath` can be used without them, as in Linux command line.

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
?- dpathw:wopen(filetype('c:'/users/_/desktop/A.jpg)).
```

Example:

Windows 10 shows a slideshow of jpg-images from desktop-folder, every users desktop is searched
```
?- dpathw:wopen(filetype('c:'/users/_/desktop/A.jpg)), sleep(5), fail;!.
```

Possible bugs: Using Swi-Prolog dicts might cause problems.
