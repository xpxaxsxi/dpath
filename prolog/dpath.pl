:- module(dpath,[
              dir/1,
              filetype/1,
              file/1,
              pathterm_atom/2,
              op(650,yfx, (:/))
          ]).
/** <module> An file system traversing utility.

Traverses directory structure and backtracks when necessary.

Example:
==
  ?- file(A/B),atom_concat(t,_,B).
==
True when A is unified to a subdirectory and B is unified to a
filename that begins with a letter t.


*/
:- use_module(library(dpathw)).

% Dict-concept is using the dot-operator
:- redefine_system_predicate( dpath:(.(_,_,_))).
.(Data, Func, Value):-  Value =.. ['.', Data,Func].

:- multifile prolog:message//1.

prolog:message( dpath(Path,Because)) -->
	       ['dpath ignoring ~w in ~q'-[Because,Path]].

%not sure if this is needed, a developer understands
%the too long paths in Windows and other exceptions
exists_directory_handle_exc(A):-
          catch(exists_directory(A),
                error(K,context(_,A)),
          ( (K=domain_error(foreign_return_value,-1))->
          fail;
          print_message(warning,dpath(A,K)),fail)).

exists_file_handle_exc(A):-
          catch(exists_file(A),
                error(K,context(_,A)),

          ( print_message(warning,dpath(A,K)),fail)).



%!        file( ?Pathterm ) is nondet.
%
%         Check if the file exists or search for a file that matches.
%
%         Example:
%         ==
%         ?- file(c:/A/'explorer.exe').
%         A = 'Windows';
%         false.
%         ==
%
%         Max path errors and other errors are shown as warnings
%
%         @error Throws errors only when debug topic
%         dpath(exceptions) is true
file(C):-
          \+compound(C),!,
          exists_file(C,cd('.')).


file( Drive:/Path):-
          atom(Drive),!,
          atom_concat(Drive,':',DriveAtom),
          file(DriveAtom/Path).

file( Drive :/ Path):-
          var(Drive),
          !,
          member(Drive,[c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),
          atom_concat(Drive,':',DriveAtom),
          file(DriveAtom/Path).

file(C):-
          compound(C),
          fold(C,A/_),
          var(A),!,
          exists_file(C,cd('.')).

file(C) :-
          compound(C),
          fold(C,A/_),
          atom(A),!,
          split_pathterm(C,Cd,Rest),
          exists_file(Rest,cd(Cd)).



%!        filetype( ?Pathterm_with_extension ) is nondet.
%
%         Check if the file exists or search for a file that matches.
%         Uses the file base name and extension. Needs a
%         dpath-qualifier because of @see ./3
%
%         Example:
%         ==
%         ?- dpath:filetype(c:/windows/A.exe).
%         A = bfsvc;
%         A = explorer;
%         ==
%
%         Max path errors and other errors are shown as warnings
%
%         @error throws errors only when debug topic
%         dpath(exceptions) is true
filetype(NotCompound):-
          \+compound(NotCompound),!,
          NotCompound=A.B,
          filetype('.'/A.B).


filetype( Drive:/Path):-
          %compound(DP),
          %DP=Drive :/ Path,
          atom(Drive),!,
          atom_concat(Drive,':',DriveAtom),
          filetype(DriveAtom/Path).

filetype( Drive:/Path):-
          %compound(DP),
          %DP=Drive :/ Path,
          var(Drive),
          !,
          member(Drive,[c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),
          atom_concat(Drive,':',DriveAtom),
          filetype(DriveAtom/Path).

filetype( C/K):-
          atom(C),
          !,
          exists_filetype(K,cd(C)).

filetype( C/K):-
          compound(C),
          fold(C,A/_),
          atom(A),
          !,
          split_pathterm(C,Cd,Rest),
          exists_filetype(Rest/K,cd(Cd)).

filetype( C/K):-
          compound(C),
          fold(C,A/_),
          var(A),
          !,
          exists_filetype(C/K,cd('.')).

filetype( C/K):-
          atom(C),!,
          exists_filetype(C/K,cd('.')).

filetype( C/K):-
          var(C),!,
          exists_filetype(C/K,cd('.')).

filetype( CK):-
          exists_filetype(CK,cd('.')).

%!        dir( ?PathTerm ) is nondet.
%
%         Check if directory exists or search for a matching
%         directory.
%
%         True if PathTerm is unified to a existing path to a
%         directory
%
%         Example:
%         ==
%         ?- dir(c:/windows/B/C).
%         B = appcompat,
%         C = appraiser ;
%         B = appcompat,
%         C = 'Programs'
%         ==
%
%         Max path errors and other errors are shown as warnings
%
%         @error throws errors only when debug topic
%         dpath(exceptions) is true
dir( DP):-
          compound(DP),
          DP=Drive :/ Path,
          atom(Drive),!,
          atom_concat(Drive,':',DriveAtom),
          dir(DriveAtom/Path).

dir( DP):-
          compound(DP),
          DP=Drive :/ Path,
          var(Drive),
          !,
          member(Drive,[c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),
          atom_concat(Drive,':',DriveAtom),
          dir(DriveAtom/Path).

dir(C):-
          compound(C),
          fold(C,A/_),
          var(A),!,
          exists_dir(C,cd('.')).

dir(C):-
          compound(C),
          fold(C,A/_),
          atom(A),!,
          split_pathterm(C,Cd,Rest),
          exists_dir(Rest,cd(Cd)).

dir( A):-
          var(A),!,
          exists_dir(A,cd('.')).

dir( A):-
          atom(A),
          exists_dir(A,cd('.')).

%exists a file that is of  some filetype
exists_filetype('/'(A,B),cd(CD)):-
          !,
          exists_file(A/X,cd(CD)),
          filename_head_tail(X,B).

exists_filetype( AB,cd(CD)):-
          exists_file(D,cd(CD)),
          filename_head_tail(D,AB).


%!        exists_file( +PathTermFile:term, -VirtualCd:term) is nondet.
%
%         PathTerm is path and VirtualCd is a virtual current directory.
%
%         Example:
%         ==
%         exists_file(File,cd('.')).
%         ==
%         - File is unified with a file from current directory.
exists_file(PathTerm,cd(CD)):-
          compound(PathTerm),!,
          PathTerm= /(PathTermyfx,File),
          fold(PathTermyfx,PathTermDir),
          exists_dir( PathTermDir,cd(CD)),
          pathterm_to_atom(CD/PathTermDir,Atom),
          directory_files2(Atom,Files),
          member(File,Files),
          \+File='.',
          \+File='..',
          pt_exists_file(CD/PathTermDir/File).

exists_file(File,cd(CD)):-
          pathterm_to_atom(CD,Atom),
          directory_files2(Atom,Files),
          member(File,Files),
          \+File='.',
          \+File='..',
          pt_exists_file(CD/File).


% doesn't handle the var(FileName)
filename_head_tail(FileName,HeadTail):-
          read_term2(FileName,HeadTail,'.').

%when in debug mode, writes out to stderr the exceptions
directory_files2(Directory,Files):-
          catch(directory_files(Directory,Files),Exc,
                (ignore( debug(dpath(exceptions),'~q',Exc)),fail)).


%!        split_pathterm( ?PathTerm, -Head,-Tail) is det.
%
%         Splits a pathterm ( +Head/?Tail)  to a Head and to a Tail
%         Head is the prefix of PathTerm
%
%         split_pathterm(H/T,S,Y) is true when S is the
%         first atom on `/' multiply separated term and Y is rest
%
%         Example:
%         ==
%         split_pathterm(a/b/c/d,a,b/c/d).
%         ==
%         Rationale: pathterm has  '/' operators that are
%         interpreted differently than usually. They are
%         interpreted as having a xfy associativity. See op/3.
split_pathterm( H/T,S,Y):-
          !,
          split_pathterm(H,Sd,Q), (atom(H),!,Y=T,S=H;Y=Q/T,S=Sd).

split_pathterm(H,H,_).


%!        fold( -PathTermyfx,+PathTermxfy) is det.
%
%         Example: folds a a/b/c to a/(b/c).
%         ==
%         ?- fold(a/b/c,a/(b/c).
%         ==
%         If the operator `/' would be redefined as
%         op(400,xfy,/) then the a/(b/c) would
%         display as a/b/c.
%
%         Example: Interprete  a/b/c as if the op(400,xfy,/)
%         is true. (The default is op(400,yfx,/) ).
%
%         Rationale: if you don't want/can't  re-define
%         the `/'-operator to op(400,xfy,/) you can use
%         this to transform terms from yfx to xfy.
%
%         PathTermyfx is an input term that will be interpreted
%         in xfy-context. PathTermxfy is output term that
%         is explicitly a xfy-term, you can't interpret it.
%         PathTermxfy is a explicitly valid xfy-term and
%         can be used in the yfx-context.
fold(KL,W):-
          compound(KL),KL=K/L,
          !,
          fold2(K,L,W).

fold(K,K).


fold2(KL,X,R):-
          compound(KL),
          !,
          /(K,L)=KL,
          fold2(K, /(L,X),R).

fold2(L,X,R):-
          R = /(L,X).



% Reads from a atom a term that is delimited multiple
% times with the Delimiter
read_term2(Atom,Term,Delimiter):-
    atomic_list_concat(List,Delimiter,Atom),
    unify_term(Delimiter,List,Term).


% generates a multiply delimited term, where
% operands are taken from List
%
% example
% ==
% ?- unify('/',[A,B,C],R).
% R=A/B/C.
% ==
unify_term(OP,List,Res):-
    length(List,Len),
    Num is Len-1,
    reverse(List,RevList),
    unify_term(Num,OP,RevList,Res).

unify_term(Num,OP,[H|T],Res):-
    Counter is Num-1,
    Num>0,
    !,
    unify_term(Counter,OP,T,ResD), Res=.. [OP,ResD,H].

unify_term(_,_,[H],H).


pt_exists_file(PathTerm):-
          pathterm_to_atom(PathTerm,Atom),
          exists_file_handle_exc(Atom).


%!        exists_dir( ?PathTerm, -VirtualCd) is nondet.
%
%         On Linux exists_dir(A,cd('/')) unifies A with
%         directories under root
exists_dir(A,cd(CD)):-
             fold(A,B),
             exists_dir2(B, cd(CD)).

exists_dir2(AB,cd(CD)):-
          compound(AB),
          AB= /(A,B),
          directory_directories(A,cd(CD)),
          exists_dir2(B,cd(CD/A)).

exists_dir2(A,cd(CD)):-
           atom(A),
           pathterm_to_atom(CD/A,Atom),
           exists_directory_handle_exc(Atom).

exists_dir2(A,cd(CD)):-
          var(A),
          directory_directories(A,cd(CD)).


% directory_directories(Main/Sub)
% traverses all directories,
% Main and Sub are atoms or variables
% cd(CD) is a virtual current directory
% Main and Sub are relative to the CD
% CD can't be a variable but can be a
% compound
directory_directories(A,cd(CD)):-
         \+compound(A),var(A),!,
         pathterm_to_atom(CD,Atom),
         filtered_directory_has_a_member(Atom,A),
         pathterm_to_atom(CD/A,AAtom),
         exists_directory_handle_exc(AAtom).

directory_directories( /(A,B),cd(CD)):-
         var(A),var(B), !,
         pathterm_to_atom(CD,Atom),
         filtered_directory_has_a_member(Atom,A),
         pathterm_to_atom(CD/A,BAtom),
         exists_directory_handle_exc(BAtom),
         directory_directories(A/B,cd(CD)).

directory_directories( /(A,B),cd(CD)):-
         atom(A),var(B),!,
         pathterm_to_atom(CD/A,Atom),
         filtered_directory_has_a_member(Atom,B),
         pathterm_to_atom(CD/A/B,BAtom),
         exists_directory_handle_exc(BAtom).

directory_directories( /(A,B),cd(CD)):-
         atom(A),atom(B),!,
         pathterm_to_atom(CD/A/B,Atom),
         exists_directory_handle_exc(Atom).

directory_directories( /(A,B),cd(CD)):-
         var(A),atom(B),!,
         pathterm_to_atom(CD,Atom),
         filtered_directory_has_a_member(Atom,A),
         pathterm_to_atom(CD/A/B,BAtom2),
         exists_directory_handle_exc(BAtom2).

directory_directories(A,cd(CD)):-
         atom(A),!,
         pathterm_to_atom(CD/A,Atom),
         exists_directory_handle_exc(Atom).



filtered_directory_has_a_member(DirAtom,Member):-
         directory_files2(DirAtom,Files),
         member(Member,Files),
         \+Member='.',
         \+Member='..'.


%!       pathterm_to_atom( Pathterm,Res) is det.
%
%        This is a private predicate does a term_to_atom/2 conversion
%        of pathterms, so that Res can be used in OS calls
%        like exists_directory/1
%
%        Example:
%        ==
%        ?- pathterm_to_atom((a/b)/c,Res).
%        Res='a/b/c'.
%        ==
%
%        Bug: Doesn't handle a/b/c.txt, but the pathterm_atom/2 does.
pathterm_to_atom( /(A,B),Res):-
         !,
         pathterm_to_atom(A,Res2),
         pathterm_to_atom(B,Res3),
         atomic_list_concat([Res2,'/',Res3],Res).

pathterm_to_atom(A,A).


%!        extterm_to_atom( -DotTerm,+Res) is det.
%
%        Res is a atom from a dot-separated filename with extension.
%
%
%        Example:
%        ==
%        extterm_to_atom(a.b.c,'a.b.c').
%        ==
extterm_to_atom( .(A,B),Res):-
         !,
         extterm_to_atom(A,Res2),
         extterm_to_atom(B,Res3),
         atomic_list_concat([Res2,'.',Res3],Res).

extterm_to_atom(A,A).

%!        pathterm_atom( ++PathTerm,-Res) is det.
%
%         Res is a atom from a path term
%         Path must contain only ground variables
%
%        Example:
%        ==
%        pathterm_atom(k/l/m/a.b,'k/l/m/a.b').
%        ==
pathterm_atom(Drive:/Path,Res):-
          !,
          pathterm_atom(Path,A),
          atomic_list_concat([Drive,':/',A],Res).

pathterm_atom( .(A,B),Res):-
         !,
         pathterm_atom(A,Res2),
         pathterm_atom(B,Res3),
         atomic_list_concat([Res2,'.',Res3],Res).

pathterm_atom( /(A,B),Res):-
         !,
         pathterm_atom(A,Res2),
         pathterm_atom(B,Res3),
         atomic_list_concat([Res2,'/',Res3],Res).

pathterm_atom(A,A).

%!        pathterm( +Len:number,+HT:list, -PT:pathterm) is semidet.
%
%         Creates a pathterm that has Len-members
%         HT is a list where  (some) members of a pathterm are
%         bind.
%
%         Example:
%         ==
%         ?- pathterm(5,[(1='c:'),(5=A.pl)],R).
%         R='c:'/X/Y/Z/A.pl.
%         ==
%
%         Usage example:
%         Find a prolog-file that is
%         under c:/users and maximally 7 subdirectories deep
%         ==
%         ?-between(3,10,DE),pathterm(DE,[(1='c:'),(2=users),(DE=A.pl)],R),filetype(R).
%         DE=5,
%         A=c,
%         R='c:'/users/'Prologist'/'OneDrive'/c.pl;
%         ==
%
pathterm(Len,HT,PT):-
          length(List,Len),
          maplist(pathterm_bind(List),HT),
          unify_term('/',List,PT).
pathterm_bind(List,Nth1=K):-
          nth1(Nth1,List,K).





