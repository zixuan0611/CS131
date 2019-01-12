% reference: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
% to transpose a matrix in prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% implement tower/3
tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(T, N),
    validarg(N, T),
    transpose(T, Trans),
    validarg(N, Trans),
    checkpos(Top, Trans),
    checkneg(Bottom, Trans),
    checkpos(Left, T),
    checkneg(Right, T),
    label(T).

label([]).
label([First|T]) :-
    fd_labeling(First),
    label(T).

validarg(N, []).
validarg(N, [Head|T]) :- 
       length(Head, N),
       fd_all_different(Head),
       fd_domain(Head, 1, N),
       validarg(N, T).

checkheights(0, [T], Height) :-
       T #< Height.

checkheights(1, [T], Height) :-
       T #> Height.

checkheights(Counts, [Head|T], Height) :-
       Head #< Height,
       checkheights(Counts, T, Height).

checkheights(Counts, [Head|T], Height) :-
       Head #> Height,
       NewCounts #= Counts-1,
       checkheights(NewCounts, T, Head).

checkpos([], []).
checkpos([Fcount|Rcount], [Flist|Tlist]) :-
       checkheights(Fcount, Flist, 0),
       checkpos(Rcount, Tlist).

checkneg([], []).
checkneg([Fcount|Rcount], [Flist|Tlist]) :-
       reverse(Flist, Rlist),
       checkheights(Fcount, Rlist, 0),
       checkneg(Rcount, Tlist).

% implement  plain_tower/3
plain_tower(N, T, C) :-
          C = counts(Top, Bottom, Left, Right),
          length(T, N),
          plaintrans(N, T, Left, Right),
          transpose(T, Trans),
          plaintrans(N, Trans, Top, Bottom),
          length(Top, N),
          length(Bottom, N),
          length(Left, N),
          length(Right, N).

special_matrix(0, []).
special_matrix(N, [Hm|T]) :-
             Nmatrix is N-1,
             Hm is N,
             special_matrix(Nmatrix, T).

checkhtplain(0, [T], Height) :-
       T < Height.

checkhtplain(1, [T], Height) :-
       T > Height.

checkhtplain(Counts, [Head|T], Height) :-
       Head < Height,
       checkhtplain(Counts, T, Height).

checkhtplain(Counts, [Head|T], Height) :-
       Head > Height,
       NewCounts is Counts-1,
       checkhtplain(NewCounts, T, Head).

% we use ! to prevent backtracking here
plaintrans(N, [], _, _).
plaintrans(N, [HHead|T], [Ha|Ta], [Hb|Tb]) :-
          length(H, N),
          special_matrix(N, H), !,
          permutation(H, HHead),
          member(Ha, H),
          checkhtplain(Ha, HHead, 0),
          reverse(HHead, RHead),
          member(Hb, H),
          checkhtplain(Hb, RHead, 0),
          plaintrans(N, T, Ta, Tb).

% implement ambiguous(N, C, T1, T2)
ambiguous(N, C, T1, T2) :-
        tower(N, T1, C),
        tower(N, T2, C),
        T1 \= T2.

% test our program and get speedup/1
test_tower(X) :-
         statistics(cpu_time, Start),
         tower(5, T, counts([2,2,1,2,5],
                [2,3,4,3,1],
                [3,4,2,1,2],
                [3,2,3,2,1])),
         statistics(cpu_time, Stop),
         nth(1, Stop, Ma),
         nth(1, Start, Mb),
         X is Ma-Mb.

test_plaintower(Y) :-
         statistics(cpu_time, Start),
         plain_tower(5, T, counts([2,2,1,2,5],
                [2,3,4,3,1],
                [3,4,2,1,2],
                [3,2,3,2,1])),
         statistics(cpu_time, Stop),
         nth(1, Stop, Ma),
         nth(1, Start, Mb),
         Y is Ma-Mb.

speedup(R) :-
         test_tower(X),
         test_plaintower(Y),
         R is Y/X.
