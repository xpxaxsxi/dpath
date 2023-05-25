# dpath 

An file system traversing utility. Backtracks in the directory structure. Got inspiration from xpath/2.

This repository is under a MIT-license. 
The latest release version can be installed to Swi-Prolog as a pack by `pack_install(dpath).` There could be pre-release versions that are under development. 

## Notes

`dpathw` and `wexplorer` are modules that interact with Windows or Linux system utilities, that is,
they can open files in a proper program using the extension. Also it is possible to open a file system browser where file is selected, both in Linux and Windows. 

Filenames are not  produced in alphabetical order from file/1 or filetype/2, the file system decides the order, usually the order of files is same. In other words, you can't trust that filenames are produced in same order always. 

Filenames and extensions are case sensitive.

If file/1 or filetype/2 or dir/1 or some underlying predicates are cached using table predicates more speed is acquired. Caching naturally can causes problems if files are deleted or current directory is changed etc. If directory structure stays same then tabling the dpath:directory_directories/2 will give lots of speed.

## Examples


#### Get files under c:/, spacebar gives alternatives
```prolog
file(c:/ File). 
```
Output could be like
```
File = bootmgr ;
File = 'BOOTNXT' ;
File = 'bootTel.dat' .
```

#### Is there a file1.txt?
```prolog
file('file1.txt'). 
```
```
true.
```


#### Show file in current directory, space gives alternatives
```prolog
file(A).
```
Output could be like
```prolog
A='file1.txt'; 
A='file2.txt';
A='file3.pl'
```


#### Show prolog files in current directory, alternatives are backtracked with spacebar
```prolog
dpath:filetype(F.pl). %Swipl doesn't like  the dot in 'F.pl' when using filetype/2 without module qualifier 
```
Output could be like
```prolog                  
F=file3
```

#### Show directory names under OneDrive root, hitting spacebar gives alternatives
```prolog
getenv('OneDrive',A),prolog_to_os_filename(ProPath,A),dir(ProPath/Subdir).
```

#### Aggregate size of all files under your Documents folder, no subdirectories included
```prolog
win_folder(personal,B), aggregate_all(sum(SZ), (file(B/C), pathterm_atom(B/C,AtomPath),size_file(AtomPath,SZ)),Res).
```

#### Aggregated size of prolog files in current directory level (no sub directories that is). Windows and Linux.
```prolog
dpath:(A.pl=X),aggregate_all(sum(Sz),(dpath:(filetype(X),pathterm_atom(X,Path)),size_file(Path,Sz)),Res).
```
Output could be like
```
X = A.pl, %example output
Res = 1349131.
```

#### Number of sub-directories in current directory. Windows and Linux.
```prolog
aggregate_all(count,dpath:dir(X),Res).
```
Output could be like
```
Res = 66.
```

#### Find ImageMagick executable 
```prolog
dir(c:/A),wildcard_match('Program Files*',A),
dpath:filetype(c:/A/Directory/Executable.exe),wildcard_match('magick*',Executable).
```

#### Open many explorer windows
Windows 10 opens up an Explorer window that has dpath.pl selected, when current
directory has only one file: the dpath.pl file. If current directory has more prolog files then 
for each file a new Explorer windows is opened.
```prolog
dpathw:wexplore(filetype(A.pl)).
``` 
Output could be like
```
%Windows or Linux shows a new file browser window for each 
A = dpath.
```

#### Open jpg images in default viewer
Windows 10 opens an jpg-image.  Next image is shown after user hits spacebar in Swi-Prolog command prompt. 
All desktops are traversed while searching for images. 
```prolog
win_folder(desktop,B),dpathw:wopen(filetype(B/A.jpg)).
```

#### Open jpg images after 5 sec delays
Windows 10 shows a slideshow of jpg-images from desktop-folder, every users desktop is searched
```prolog
win_folder(desktop,DesktopPath),dpathw:wopen(filetype(DesktopPath/A.jpg)), sleep(5), fail;!.
```

#### Linux example, show files that are at 6th directory level under root
```prolog
dir('/'/A/B/C/D/E/F).
```
```
A = usr,
B = include,
C = 'c++',
D = '4.7',
E = ext,
F = pb_ds
```

#### Linux example, is there a smb.conf in 3rd directory level? 
```prolog
file('/'/A/B/'smb.conf').
```
```
A = etc,
B = samba 
```

#### Possible bugs
Using Swi-Prolog dicts might cause problems
