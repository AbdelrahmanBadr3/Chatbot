res([FirstWord|RestOfSentence]) :-
  reSe([FirstWord|RestOfSentence]).

reSe([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord(Char,FirstWord,NextChar),
  readRestOfSentence(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res -------------------------
   readRestOfSentence(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence(_,Char,[NextWord|RestOfSentence]) :-
     readWord(Char,NextWord,NextChar),
     readRestOfSentence(NextWord,NextChar,RestOfSentence).

   readWord(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord(Char,Word,NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord(_,Word,NextChar) :-
     get0(TempChar),
     readWord(TempChar,Word,NextChar).

   restWord(Char,[NewChar|RestWord],NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar).
     restWord(Char,[],Char).

   singleCharWord(44).  /* , */
   singleCharWord(59).  /* ; */
   singleCharWord(58).  /* : */
   singleCharWord(63).  /* ? */
   singleCharWord(33).  /* ! */
   singleCharWord(46).  /* . */

   componentChar(Char,Char) :- Char>96,Char<123.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar(Char,Char) :- Char>47,Char<58.
   componentChar(39,39).  /* ' */
   componentChar(45,45).  /* - */
   componentChar(95,95).  /* _ */

   endOfSentenceWord('.').
   endOfSentenceWord('!').
   endOfSentenceWord('?').

%-------------------------------------------------------------
% res_pc(-Sentence)
%-------------------------------------------------------------

res_pc([FirstWord|RestOfSentence]) :-
  reSe_pc([FirstWord|RestOfSentence]).

reSe_pc([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord_pc(Char,FirstWord,NextChar),
  readRestOfSentence_pc(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res_pc -------------------------
   readRestOfSentence_pc(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence_pc(_,Char,[NextWord|RestOfSentence]) :-
     readWord_pc(Char,NextWord,NextChar),
     readRestOfSentence_pc(NextWord,NextChar,RestOfSentence).

   readWord_pc(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord_pc(Char,Word,NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord_pc(_,Word,NextChar) :-
     get0(TempChar),
     readWord_pc(TempChar,Word,NextChar).

   restWord_pc(Char,[NewChar|RestWord],NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar).
     restWord_pc(Char,[],Char).

   componentChar_pc(Char,Char) :- Char>96,Char<123.

   componentChar_pc(Char,Char) :- Char>64,Char<91.

   componentChar_pc(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar_pc(Char,Char) :- Char>47,Char<58.
   componentChar_pc(39,39).  /* ' */
   componentChar_pc(45,45).  /* - */
   componentChar_pc(95,95).  /* _ */

%-------------------------------------------------------------
% ws(+Sentence)
%-------------------------------------------------------------

ws([F|R]) :-
   write(F),
   wrs(R).

   %--- ancillaries to ws ------------------------
   wrs([F|R]) :-
     write(' '),
     write(F),
     wrs(R).
   wrs([]).

%-------------------------------------------------------------
% space/0
%-------------------------------------------------------------

space :- write(' ').

%-------------------------------------------------------------
% rs(-String)
%-------------------------------------------------------------

rs(S) :-
   get0(C),
   (
      C == -1,  S = [], !, fail;
      C == 10,  S = [], ! ;
      C == 32, !, rs(S);
      !, rs(C,S)
   ).

rs(C,[C|Cs]) :-
   get0(D),
   (
      D == -1,  Cs = [], !, fail;
      D == 10,  Cs = [], ! ;
      D == 32,  Cs = [], ! ;
      !, rs(D,Cs)
   ).


%-------------------------------------------------------------
% wrst(+String)
%-------------------------------------------------------------

wrst([]) :- !.
wrst([C|Cs]) :- put(C), wrst(Cs).
%-------------------------------------------------------------
:-discontiguous(prop/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% VEGETABLES %%%%

prop(tomato,is,vegetable).
prop(onion,is,vegetable).
prop(bell_pepper,is,vegetable).
prop(chili_pepper,is,vegetable).
prop(carrot,is,vegetable).
prop(pea,is,vegetable).
prop(artichoke,is,vegetable).
prop(eggplant,is,vegetable).
prop(cucumber,is,vegetable).
prop(lettuce,is,vegetable).
prop(okra,is,vegetable).
prop(cauliflower,is,vegetable).
prop(cabbage,is,vegetable).
prop(broccoli,is,vegetable).
prop(mushroom,is,vegetable).
prop(potato,is,vegetable).
prop(zucchini,is,vegetable).
prop(broccoli,is,vegetable).
prop(spinach,is,vegetable).
prop(corn,is,vegetable).

%%%% FRUITS %%%%

prop(strawberry,is,fruit).
prop(blackberry,is,fruit).
prop(blueberry,is,fruit).
prop(banana,is,fruit).
prop(orange,is,fruit).
prop(grape,is,fruit).
prop(pineapple,is,fruit).
prop(apple,is,fruit).
prop(kiwi,is,fruit).
prop(peaches,is,fruit).
prop(guava,is,fruit).
prop(pear,is,fruit).
prop(mango,is,fruit).
prop(apricot,is,fruit).
prop(avocado,is,fruit).
prop(cherry,is,fruit).
prop(fig,is,fruit).
prop(coconut,is,fruit).
prop(lemon,is,fruit).
prop(watermelon,is,fruit).
prop(cantaloupe,is,fruit).

%%%% DIARY %%%%

prop(cheese,is,diary).
prop(milk,is,diary).
prop(yogurt,is,diary).

%%%% CARBS %%%%

prop(flour,is,carb).
prop(rice,is,carb).
prop(pasta,is,carb).
prop(chocolate,is,carb).

%%%% FATS %%%%

prop(oil,is,fat).
prop(butter,is,fat).

%%%% PROTEINS %%%%

prop(egg,is,protein).
prop(fish,is,protein).
prop(chicken,is,protein).
prop(meat,is,protein).
prop(shrimp,is,protein).
prop(minced_meat,is,protein).

%%%% DRESSING %%%%

prop(mayonnaise,is,dressing).
prop(vinegar,is,dressing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(chicken_caesar_salad,contain,chicken).
prop(chicken_caesar_salad,contain,oil).
prop(chicken_caesar_salad,contain,lettuce).
prop(chicken_caesar_salad,contain,cheese).
prop(chicken_caesar_salad,contain,mayonnaise).
prop(chicken_caesar_salad,contain,vinegar).
prop(chicken_caesar_salad,contain,bread).

prop(green_salad,contain,carrot).
prop(green_salad,contain,bell_pepper).
prop(green_salad,contain,lettuce).
prop(green_salad,contain,onion).
prop(green_salad,contain,tomato).
prop(green_salad,contain,cucumber).

prop(coleslaw_salad,contain,carrot).
prop(coleslaw_salad,contain,cabbage).
prop(coleslaw_salad,contain,mayonnaise).
prop(coleslaw_salad,contain,oil).

prop(pasta_salad,contain,bell_pepper).
prop(pasta_salad,contain,mayonnaise).
prop(pasta_salad,contain,pasta).
prop(pasta_salad,contain,corn).

prop(fruit_salad,contain,strawberry).
prop(fruit_salad,contain,banana).
prop(fruit_salad,contain,orange).
prop(fruit_salad,contain,apple).

prop(croissant,contain,butter).
prop(croissant,contain,flour).
prop(croissant,contain,milk).
prop(croissant,contain,oil).
prop(croissant,contain,egg).

prop(spanish_omelette,contain,egg).
prop(spanish_omelette,contain,oil).
prop(spanish_omelette,contain,potato).

prop(boiled_egg,contain,egg).

prop(grilled_chicken,contain,chicken).
prop(grilled_chicken,contain,lemon).
prop(grilled_chicken,contain,onion).

prop(fried_chicken,contain,chicken).
prop(fried_chicken,contain,oil).
prop(fried_chicken,contain,onion).
prop(fried_chicken,contain,flour).

prop(cake,contain,flour).
prop(cake,contain,butter).
prop(cake,contain,milk).
prop(cake,contain,egg).

prop(chocolate_cake,contain,cake).
prop(chocolate_cake,contain,chocolate).

prop(white_rice,contain,rice).
prop(white_rice,contain,butter).

prop(mexican_rice,contain,rice).
prop(mexican_rice,contain,oil).
prop(mexican_rice,contain,onion).
prop(mexican_rice,contain,tomato).

prop(ratatouille,contain,zucchini).
prop(ratatouille,contain,eggplant).
prop(ratatouille,contain,tomato).
prop(ratatouille,contain,bell_pepper).
prop(ratatouille,contain,onion).
prop(ratatouille,contain,lemon).
prop(ratatouille,contain,oil).
prop(ratatouille,contain,vinegar).

prop(lasagne,contain,pasta).
prop(lasagne,contain,milk).
prop(lasagne,contain,flour).
prop(lasagne,contain,butter).
prop(lasagne,contain,minced_meat).
prop(lasagne,contain,cheese).

prop(pasta_white_sauce,contain,pasta).
prop(pasta_white_sauce,contain,milk).
prop(pasta_white_sauce,contain,flour).
prop(pasta_white_sauce,contain,butter).

prop(pasta_red_sauce,contain,pasta).
prop(pasta_red_sauce,contain,tomato).
prop(pasta_red_sauce,contain,oil).

prop(pasta_alfredo,contain,pasta).
prop(pasta_alfredo,contain,milk).
prop(pasta_alfredo,contain,flour).
prop(pasta_alfredo,contain,butter).
prop(pasta_alfredo,contain,chicken).

prop(pasta_negresco,contain,pasta).
prop(pasta_negresco,contain,milk).
prop(pasta_negresco,contain,flour).
prop(pasta_negresco,contain,butter).
prop(pasta_negresco,contain,chicken).
prop(pasta_negresco,contain,cheese).

prop(shrimp_pasta,contain,pasta).
prop(shrimp_pasta,contain,shrimp).
prop(shrimp_pasta,contain,butter).
prop(shrimp_pasta,contain,milk).

prop(pizza,contain,tomato).
prop(pizza,contain,cheese).
prop(pizza,contain,flour).
prop(pizza,contain,oil).

prop(bread,contain,milk).
prop(bread,contain,flour).
prop(bread,contain,butter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(tomato,contain,11,cal).
prop(onion,contain,44,cal).
prop(cheese,contain,431,cal).
prop(egg,contain,78,cal).
prop(pasta,contain,131,cal).
prop(bell_pepper,contain,24,cal).
prop(chili_pepper,contain,18,cal).
prop(carrot,contain,25,cal).
prop(pea,contain,81,cal).
prop(artichoke,contain,120,cal).
prop(eggplant,contain,25,cal).
prop(cucumber,contain,32,cal).
prop(lettuce,contain,15,cal).
prop(okra,contain,33,cal).
prop(cauliflower,contain,25,cal).
prop(cabbage,contain,25,cal).
prop(broccoli,contain,31,cal).
prop(mushroom,contain,5,cal).
prop(potato,contain,163,cal).
prop(zucchini,contain,33,cal).
prop(spinach,contain,23,cal).
prop(corn,contain,86,cal).
prop(strawberry,contain,33,cal).
prop(blackberry,contain,43,cal).
prop(blueberry,contain,57,cal).
prop(banana,contain,89,cal).
prop(orange,contain,47,cal).
prop(grape,contain,62,cal).
prop(pineapple,contain,42,cal).
prop(apple,contain,92,cal).
prop(kiwi,contain,42,cal).
prop(peaches,contain,59,cal).
prop(guava,contain,38,cal).
prop(pear,contain,85,cal).
prop(mango,contain,99,cal).
prop(apricot,contain,48,cal).
prop(avocado,contain,160,cal).
prop(cherry,contain,50,cal).
prop(fig,contain,107,cal).
prop(coconut,contain,283,cal).
prop(lemon,contain,24,cal).
prop(watermelon,contain,30,cal).
prop(cantaloupe,contain,34,cal).
prop(milk,contain,124,cal).
prop(yogurt,contain,218,cal).
prop(flour,contain,364,cal).
prop(rice,contain,150,cal).
prop(oil,contain,240,cal).
prop(butter,contain,204,cal).
prop(fish,contain,305,cal).
prop(chicken,contain,335,cal).
prop(meat,contain,250,cal).
prop(shrimp,contain,85,cal).
prop(minced_meat,contain,332,cal).
prop(mayonnaise,contain,188,cal).
prop(vinegar,contain,3,cal).
prop(chocolate,contain,137,cal).
%prop(,contain,,cal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(cheese,not,lunch).
prop(yogurt,not,lunch).
prop(boiled_egg,not,lunch).
prop(boiled_egg,not,dinner).
prop(spanish_omelette,not,lunch).
prop(spanish_omelette,not,dinner).
prop(croissant,not,lunch).
prop(chicken_caesar_salad,not,breakfast).
prop(chicken_caesar_salad,not,dinner).
prop(pizza,not,breakfast).
prop(shrimp_pasta,not,breakfast).
prop(shrimp_pasta,not,dinner).
prop(pasta_negresco,not,breakfast).
prop(pasta_negresco,not,dinner).
prop(pasta_alfredo,not,breakfast).
prop(pasta_alfredo,not,dinner).
prop(pasta_red_sauce,not,breakfast).
prop(pasta_red_sauce,not,dinner).
prop(pasta_white_sauce,not,breakfast).
prop(pasta_white_sauce,not,dinner).
prop(fried_chicken,not,breakfast).
prop(fried_chicken,not,dinner).
prop(grilled_chicken,not,breakfast).
prop(grilled_chicken,not,dinner).
prop(lasagne,not,breakfast).
prop(lasagne,not,dinner).
prop(ratatouille,not,breakfast).
prop(ratatouille,not,dinner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   






%-------------------------------------------------------------
 intersect([X|Y],M,[X|Z]) :-
  member(X,M),
  intersect(Y,M,Z). 
 intersect([X|Y],M,Z) :- 
  \+ member(X,M),
  intersect(Y,M,Z).
 intersect([],_,[]).
 
 
 remove(_,[],[]).
 remove(X,[X|R],R2):- 
    remove(X,R,R2). 
 remove(X,[F|R],[F|S]):-
    X\=F, 
	remove(X,R,S).
 remove_list([],R,R).
 remove_list([X|Y],M,R):-
   remove(X,M,R1),
   remove_list(Y,R1,R).
%-------------------------------------------------------------
isValid([how,many,calories,does,_,contain]).
isValid([what,does,_,contain]).
isValid([can,i,have,_,for,_]).
isValid([what,is,_]).
isValid([how,many,calories,do,i,have,left]).
isValid([what,kind,of,_,does,_,contain]).
isValid([is,_,a,_,in,_]).
isValid([what,can,i,have,for,_,that,contains,_]).
isValid([i,ate,_,for,_]).
isValid([i,do,not,eat,_]).

w(["I","can","not","understand","you"]).
%---------------------------------

%---------------------------------
filterProp(Relation,Result):-
   Z=prop(X,Relation,Y),
   O=	(X,Y),
   setof(O,Z,Result).
%---------------------------------
matchFirst(_,[],[]).

matchFirst(T1,LF,LM):-
LF=[(X,Y)|T],
T1=X,
LM=[E-Occ|T2],
E=Y,
Occ=1,
matchFirst(T1,T,T2).

matchFirst(T1,LF,LM):-
LF=[(X,Y)|T],
T1\=X,
LM=[E-Occ|T2],
E=Y,
Occ=0,
matchFirst(T1,T,T2).
%---------------------------------
matchSecond(_,[],[]).

matchSecond(T1,LF,LM):-
LF=[(X,Y)|T],
T1=Y,
LM=[E-Occ|T2],
E=X,
Occ=1,
matchSecond(T1,T,T2).

matchSecond(T1,LF,LM):-
LF=[(X,Y)|T],
T1\=Y,
LM=[E-Occ|T2],
E=X,
Occ=0,
matchSecond(T1,T,T2).

%---------------------------------

getmatches(_,[],[]).

getmatches(E-Occ,[E-Occ2|T],[E-Occ2|NT]):-
getmatches(E-Occ,T,NT).

getmatches(E-Occ,[X-_|T],NT):-
X\=E,
getmatches(E-Occ,T,NT).

mergeMatchLists(ML1,ML2,Merged):-
mergeHelp(ML1,N1),
mergeHelp(ML2,N2),
mergeHelp2(N1,N2,Merged).


mergeHelp2([],_,[]).
mergeHelp2([E-Occ|T1],Ml2,[E-SOcc|TR2]):-
member(E-_,Ml2),
getmatches(E-Occ,Ml2,TR),
sumOcc([E-Occ|TR],SOcc),
mergeHelp2(T1,Ml2,TR2).
mergeHelp2([E-Occ|T1],Ml2,[E-Occ|TR2]):-
 \+ member(E-_,Ml2),
mergeHelp2(T1,Ml2,TR2).



mergeHelp([X],[X]).

mergeHelp([],[]).

mergeHelp([E-Occ|T],[E-SOcc|FNT]):-
getmatches(E-Occ,[E-Occ|T],L),
sumOcc(L,SOcc),
remove_list(L,[E-Occ|T],NT),
mergeHelp(NT,FNT).


sumOcc([],0).
sumOcc([_-Occ2|T],S):-
sumOcc(T,Ts),
S is Ts+Occ2.

%---------------------------------
maxOcc([_-OC],OC).
maxOcc([_-Occ,_-Occ2|T],C):-
Max is max(Occ,Occ2),
maxOcc([_-Max|T],C).
 
 bestMatchesMin([],_,[]).
bestMatchesMin([E-Occ|T],Occ,[E|NT]):-
bestMatchesMin(T,Occ,NT).

bestMatchesMin([_-Occ1|T],Occ,NT):-
Occ1 <Occ,
bestMatchesMin(T,Occ,NT).

bestMatches(L,Resualt):-
maxOcc(L,Occ),
bestMatchesMin(L,Occ,Resualt).




%---------------------------------
foodhelp([],0).
foodhelp([H|T],Resualt):-
foodhelp(T,Resualt2),
foodCal(H,Temp),
Resualt is Temp+Resualt2.

foodCal(F,C):-
prop(F,_,C,_).

foodCal(F,C):-
setof(X,prop(F,contain,X),L),
foodhelp(L,C).
%---------------------------------

foodCalList([],0).
foodCalList(FL,C):-
FL=[H|T],
foodCal(H,TC),
foodCalList(T,OC),
C is TC+OC.

%------------------------------------
calcCalories(Q,L,L1,Resualt):-
calcCaloriesHelp(Q,L,L1,0,Resualt).

calcCaloriesHelp(Q,[],[],AC,Resualt):-
foodCal(Q,C),
Resualt is 1800 -  C - AC.
calcCaloriesHelp(Q,[H|T],[H1|T1],Ac,X):-
H=[i,ate,W,for,_],
H1=["Ok"],
foodCal(W,C),
AcN is Ac+C,
calcCaloriesHelp(Q,T,T1,AcN,X).

calcCaloriesHelp(Q,[H|T],[H1|T1],Ac,X):-
(H\=[i,ate,_,for,_];
H1\=["Ok"]),
calcCaloriesHelp(Q,T,T1,Ac,X).   
%--------------------------------------

getDiffAnswer(_,[],[],[H|_],H).
getDiffAnswer(Q,[PQ|TPQ],[PR|TPR],[CR|TCR],R):-
Q=PQ,
remove_list(PR,[CR|TCR],NCR),
getDiffAnswer(Q,TPQ,TPR,NCR,R).

getDiffAnswer(Q,[PQ|TPQ],[_|TPR],[CR|TCR],R):-
Q\=PQ,
getDiffAnswer(Q,TPQ,TPR,[CR|TCR],R).

%---------------------------------

response(Q,PQ,PR, L):-
setof(R,response2(Q,PQ,PR,R),[L|_]).

%A) How many calories does X contain? 
 response2(Q,PQ,PR,["I",do,not,know]):-
  Q = [how,many,calories,does,Fc,contain], 
 ((\+ prop(_,_,Fc)); 
 (\+prop(Fc,_,_));(\+prop(Fc,_,_,_))),
 getDiffAnswer(Q,PQ,PR,["I",do,not,know],_). 
 
  response2(Q,PQ,PR,["I",told,you,that,before]):-
  Q = [how,many,calories,does,Fc,contain], 
 ((\+ prop(_,_,Fc)); 
 (\+prop(Fc,_,_));(\+prop(Fc,_,_,_))),
 \+getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).

 response2(Q,PQ,PR,[C, "Calories"]):-
 Q = [how,many,calories,does,Fc,contain], 
 prop(Fc,_,_),
 foodCal(Fc,C), 
 getDiffAnswer(Q,PQ,PR,[C],_).
 
 
 response2(Q,PQ,PR,["I",told,you,that,before]):-
 Q = [how,many,calories,does,Fc,contain], 
 prop(Fc,_,_),
 foodCal(Fc,C), 
 \+getDiffAnswer(Q,PQ,PR,[C, "Calories"],_).


%---------------------------
% B)What does X contain?
 response2(Q,_,_,["I",do,not,know]) :-
 Q = [what,does,Fc,contain], 
 (\+prop(Fc,_,_)).
 
 response2(Q,PQ,PR,[R]) :-
 Q=[what,does,Fc,contain],  
 prop(Fc,_,_),
 filterProp(contain,L1),
 matchFirst(Fc,L1,R1), 
 bestMatchesMin(R1,1,CR), 
 length(CR,N), 
 N >= 1, 
 getDiffAnswer(Q,PQ,PR,CR,R). 
 
 response2(Q,PQ,PR,["I", told, you, that, before]) :-
 Q=[what,does,Fc,contain],  
 prop(Fc,_,_),
 filterProp(contain,L1),
 matchFirst(Fc,L1,R1), 
 bestMatchesMin(R1,1,CR), 
 length(CR,N), 
 N >= 1, 
 \+getDiffAnswer(Q,PQ,PR,CR,_). 

%---------------------------------
%c)Can i have X for Y ?
 response2(Q,PQ,PR, [X, "is", not, suitable, for, Y]):-
 Q=[can,i,have,X,for,Y],
 prop(X,not,Y),
 getDiffAnswer(Q,PQ,PR,[X, "is", not, suitable, for, Y],_).
 
 
 
 response2(Q,PQ,PR, ["I", told, you, that, before]):-
 Q=[can,i,have,X,for,Y],
 prop(X,not,Y),
 \+getDiffAnswer(Q,PQ,PR,[X, "is", not, suitable, for, Y],_).
 
 response2(Q,PQ,PR,["You", can, have, X, for, Y]):-
 Q=[can,i,have,X,for,Y],
 \+prop(X,not,Y),
 prop(X,_,_),
 prop(_,not,Y),
 calcCalories(X,PQ,PR,C),
 C>=0,
 getDiffAnswer(Q,PQ,PR,["You", can, have, X, for, Y],_).
 
 
 response2(Q,PQ,PR,["I", told, you, that, before]):-
 Q=[can,i,have,X,for,Y],
 \+prop(X,not,Y),
 prop(X,_,_),
 prop(_,not,Y),
 calcCalories(X,PQ,PR,C),
 C>=0,
 \+	getDiffAnswer(Q,PQ,PR,["You", can, have, X, for, Y],_).
 
 
 response2(Q,PQ,PR, ["No"]):-
 Q=[can,i,have,X,for,Y],
 \+prop(X,not,Y),
 prop(X,_,_),
 prop(_,not,Y),
 calcCalories(X,PQ,PR,C),
 C<0,
 getDiffAnswer(Q,PQ,PR,["No"],_).
 
 
 response2(Q,PQ,PR,["I", told, you, that, before]):-
 Q=[can,i,have,X,for,Y],
 \+prop(X,not,Y),
 prop(X,_,_),
 prop(_,not,Y),
 calcCalories(X,PQ,PR,C),
 C<0,
 \+getDiffAnswer(Q,PQ,PR,["No"],_).
 
 response2(Q,PQ,PR, ["I", do, not, know]):-
 Q=[can,i,have,X,for,_],
 \+prop(X,_,_),
 getDiffAnswer(Q,PQ,PR,["I", do, not, know],_).
 
 
 
 response2(Q,PQ,PR, ["I", told, you, that, before]):-
 Q=[can,i,have,X,for,_],
 \+prop(X,_,_),
 \+getDiffAnswer(Q,PQ,PR,["I", do, not, know],_).
 
 
 
 response2(Q,PQ,PR,["I", do, not, know]):-
 Q=[can,i,have,X,for,Y],
 \+prop(X,not,Y),
 prop(X,_,_),
 prop(_,not,Y),
 \+calcCalories(X,PQ,PR,_),
 getDiffAnswer(Q,PQ,PR,["I", do, not, know],_).
 
 
 response2(Q,PQ,PR,["I", told, you, that, before]):-
 Q=[can,i,have,X,for,Y],
 \+prop(X,not,Y),
 prop(X,_,_),
 prop(_,not,Y),
 \+calcCalories(X,PQ,PR,_),
 \+	getDiffAnswer(Q,PQ,PR,["I", do, not, know],_).
  
%-----------------------------------------------
%D)What is X?
response2(Q,PQ,PR,["I",do,not,know]) :-
Q=[what,is,X],
\+prop(X,is,_),
getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).

response2(Q,PQ,PR,["I", told, you, that, before]) :-
Q=[what,is,X],
\+prop(X,is,_),
\+getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).

response2(Q,PQ,PR,["I", told, you, that, before]) :-
Q=[what,is,X],
prop(X,is,CR),
\+getDiffAnswer(Q,PQ,PR,[CR],_).

response2(Q,PQ,PR,CR) :-
Q=[what,is,X],
prop(X,is,CR),
getDiffAnswer(Q,PQ,PR,[CR],_).
%---------------------------------
%E)How many calories do I have left?
response2(Q,PQ,PR,[Nc,"Calories"]) :-
Q=[how,many,calories,do,i,have,left],
foodFromHistory(PQ,L),
foodCalList(L,C),
Nc is 1800 - C,
getDiffAnswer(Q,PQ,PR,[Nc,"Calories"],_).

response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q=[how,many,calories,do,i,have,left],
foodFromHistory(PQ,L),
foodCalList(L,C),
Nc is 1800 - C,
\+getDiffAnswer(Q,PQ,PR,[Nc,"Calories"],_).

response2(Q,PQ,PR,["I",do,not,know]) :-
Q=[how,many,calories,do,i,have,left],
foodFromHistory(PQ,L),
\+foodCalList(L,_),
getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).

response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q=[how,many,calories,do,i,have,left],
foodFromHistory(PQ,L),
\+foodCalList(L,_),
\+getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).
%---------------------------------
%F) What kind of X does Y contain? Given sample

response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q = [what,kind,of,FC,does,F,contain], 
prop(_,_,FC), prop(F,_,_), 
filterProp(contain,L1), 
filterProp(is,L2),
 matchFirst(F,L1,R1), 
 matchSecond(FC,L2,R2), 
 mergeMatchLists(R1,R2,L3), 
 bestMatchesMin(L3,2,CR), 
 length(CR,N),
 N >= 1,
 \+getDiffAnswer(Q,PQ,PR,CR,_).
 
 
 response2(Q,_,_,["I",do,not,know]) :-
 Q = [what,kind,of,FC,does,F,contain], 
 ((\+ prop(_,_,FC)); 
 (\+prop(F,_,_))).
 
 
 
 response2(Q,_,_,["Nothing",from,what,i,know]) :-
 Q = [what,kind,of,FC,does,F,contain], 
 prop(_,_,FC), prop(F,_,_), 
 filterProp(contain,L1),
 filterProp(is,L2),
 matchFirst(F,L1,R1), 
 matchSecond(FC,L2,R2),
 mergeMatchLists(R1,R2,L3), 
 bestMatchesMin(L3,2,CR),
 length(CR,0).
 
 
 response2(Q,PQ,PR,[R]) :-
 Q = [what,kind,of,FC,does,F,contain], 
 prop(_,_,FC), 
 prop(F,_,_),
 filterProp(contain,L1), 
 filterProp(is,L2), 
 matchFirst(F,L1,R1), 
 matchSecond(FC,L2,R2), 
 mergeMatchLists(R1,R2,L3), 
 bestMatchesMin(L3,2,CR), 
 length(CR,N), N >= 1,
 getDiffAnswer(Q,PQ,PR,CR,R).
 
%---------------------------------
%G)is X a Y  in Z?
response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q=[is,X,a,Y,in,Z],
prop(X,is,Y),
prop(Z,contain,X),
\+getDiffAnswer(Q,PQ,PR,["Yes"],_).

response2(Q,PQ,PR,["Yes"]) :-
Q=[is,X,a,Y,in,Z],
prop(X,is,Y),
prop(Z,contain,X),
getDiffAnswer(Q,PQ,PR,["Yes"],_).

response2(Q,PQ,PR,["No"]) :-
Q=[is,X,a,Y,in,Z],
prop(X,is,Y),
\+prop(Z,contain,X),
getDiffAnswer(Q,PQ,PR,["NO"],_).

response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q=[is,X,a,Y,in,Z],
prop(X,is,Y),
\+prop(Z,contain,X),
\+getDiffAnswer(Q,PQ,PR,["NO"],_).


response2(Q,PQ,PR,["No"]) :-
Q=[is,X,a,Y,in,Z],
\+prop(X,is,Y),
prop(Z,contain,X),
getDiffAnswer(Q,PQ,PR,["NO"],_).

response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q=[is,X,a,Y,in,Z],
\+prop(X,is,Y),
prop(Z,contain,X),
\+getDiffAnswer(Q,PQ,PR,["NO"],_).

response2(Q,PQ,PR,["I",do,not,know]) :-
Q=[is,X,a,Y,in,Z],
\+prop(X,is,Y),
\+prop(Z,contain,X),
getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).

response2(Q,PQ,PR,["I",told,you,that,before]) :-
Q=[is,X,a,Y,in,Z],
\+prop(X,is,Y),
\+prop(Z,contain,X),
\+getDiffAnswer(Q,PQ,PR,["I",do,not,know],_).

%---------------------------------
 remove_list2([],R,R).
 remove_list2([H|Y],M,R):-
    H=[X],
   remove(X,M,R1),
   remove_list2(Y,R1,R).

removeunLiked([],R2,R2).

removeunLiked(PQ,R2,L):-
getUnlikedIngredients(PQ,ULI),
remove_list2(ULI,R2,Temp),
remove_contain(ULI,Temp,L).
remove_contain([],R,R).


remove_contain([H|T],[H1|T1],R):-
H=[B],
remove_help(B,[H1|T1],NEH),
remove_contain(T,NEH,R).
remove_help(_,[],[]).

remove_help(I,[H1|T],R):-
prop(H1,contain,I),
remove_help(I,T,R).

remove_help(I,[H1|T],[H1|R]):-
\+prop(H1,contain,I),
prop(_,contain,I),
remove_help(I,T,R).

remove_help(I,[H1|T],R):-
\+prop(_,contain,I),
setof(V,prop(H1,contain,V),L),
setof(V1,prop(I,contain,V1),L1),
intersect(L,L1,R1),
length(R1,C),
C>=1,
remove_help(I,T,R).

remove_help(I,[H1|T],[H1|R]):-
\+prop(_,contain,I),
setof(V,prop(H1,contain,V),L),
setof(V1,prop(I,contain,V1),L1),
intersect(L,L1,R1),
length(R1,C),
C=0,
remove_help(I,T,R).





%---------------------------------
%H)what can i have for Y that contains X ?
response2(Q,PQ,PR,Rs) :-
Q=[what,can,i,have,for,Y,that,contains,X],
prop(_,_,Y),
prop(X,_,_),
filterProp(not,L),
matchSecond(Y,L,R),
bestMatchesMin(R,1,NR),
filterProp(contain,L1),
matchSecond(X,L1,R1),
bestMatchesMin(R1,1,NR1),
remove_list(NR,NR1,R2),
removeunLiked(PQ,R2,NR2),
getDiffAnswer(Q,PQ,PR,NR2,Rs).

response2(Q,PQ,PR,["I", told, you, that, before]) :-
Q=[what,can,i,have,for,Y,that,contains,X],
prop(_,_,Y),
prop(X,_,_),
filterProp(not,L),
matchSecond(Y,L,R),
bestMatchesMin(R,1,NR),
filterProp(contain,L1),
matchSecond(X,L1,R1),
bestMatchesMin(R1,1,NR1),
remove_list(NR,NR1,R2),
removeunLiked(PQ,R2,NR2),
length(NR2,N),
N>=1,
\+getDiffAnswer(Q,PQ,PR,NR2,_).


response2(Q,PQ,PR,["Nothing", from, what, i, know]) :-
Q=[what,can,i,have,for,Y,that,contains,X],
prop(_,_,Y),
prop(X,_,_),
filterProp(not,L),
matchSecond(Y,L,R),
bestMatchesMin(R,1,NR),
filterProp(contain,L1),
matchSecond(X,L1,R1),
bestMatchesMin(R1,1,NR1),
remove_list(NR,NR1,R2),
removeunLiked(PQ,R2,NR2),
length(NR2,N),
N=0,
\+getDiffAnswer(Q,PQ,PR,NR2,_).

response2(Q,_,_,["I", do, not, know]) :-
Q=[what,can,i,have,for,Y,that,contains,X],
\+prop(_,contain,X),
prop(_,_,Y).





%---------------------------------
maxOcc2([],0).
maxOcc2([_-OC],OC).
maxOcc2([_-Occ,_-Occ2|T],C):-
Max is max(Occ,Occ2),
maxOcc2([_-Max|T],C).

sortWithOcc(M,[],[]):-
M\=0.
sortWithOcc(M,[H-M|T],[H-M|T2]):-
M\=0,
sortWithOcc(M,T,T2).
sortWithOcc(M,[_-X|T],T2):-
X\=M,
sortWithOcc(M,T,T2).


listOrderDesc(L,R):-
maxOcc2(L,MOC),
listOrderHelp(MOC,L,R).


listOrderHelp(_,[],[]).

listOrderHelp(M,L,[H|T]):-
sortWithOcc(M,L,H),
remove_list(H,L,L1),
maxOcc2(L1,M1),
listOrderHelp(M1,L1,T).
%---------------------------------
foodFromHistory(X,R):-
foodFromHelper(X,R).
  
validfood(H,X):-
H=[you,can,have,X,for,_];
H=[i,ate,X,for,_].

foodFromHelper([],[]).
foodFromHelper([HL|THL],[X|NT]):-
 validfood(HL,X),
foodFromHelper(THL,NT).

foodFromHelper([HL|THL],NT):-
 \+ validfood(HL,_),
foodFromHelper(THL,NT).
  


%---------------------------------

getUnlikedIngredients([],[]).

getUnlikedIngredients([HL|THL],[L|T]):-
HL=[i,do,not,eat,X],
setof(Z,prop(X,contain,Z),L),
getUnlikedIngredients(THL,T).

getUnlikedIngredients([HL|THL],[[X]|T]):-
HL=[i,do,not,eat,X],
prop(X,is,_),
getUnlikedIngredients(THL,T).

getUnlikedIngredients([HL|THL],T):-
HL\=[i,do,not,eat,_],
getUnlikedIngredients(THL,T).

%---------------------------------
printTail([]).
printTail([H|T]):-
ws(["And",H]),
printTail(T).

printlast([],X):-
ws(["You",had,'-',for,X]).

printlast([H],X):-
ws(["You",had,H,for,X]).

printlast([H|T],X):-
ws(["You",had,H]),
length(T,C),
C>=1,
printTail(T),
ws([for,X]).

displayAc(QAc,RAc):-
displayQAc(QAc,R1),
displayRAc(RAc,R2),
append(R1,R2,R3),
matchSecond(breakfast,R3,A),
bestMatchesMin(A,1,An),
matchSecond(lunch,R3,A1),
bestMatchesMin(A1,1,An1),
matchSecond(dinner,R3,A2),
bestMatchesMin(A2,1,An2),
printlast(An,breakfast),
writeln(""),
printlast(An1,lunch),
writeln(""),
printlast(An2,dinner),
writeln("").

displayQAc([],[]).

displayQAc([H|T],[(X,Y)|Rest]):-
H=[i,ate,X,for,Y],
displayQAc(T,Rest).

displayQAc([H|T],Rest):-
H\=[i,ate,_,for,_],
displayQAc(T,Rest).



displayRAc([],[]).

displayRAc([H|T],[(X,Y)|Rest]):-
H=["You", can, have, X, for, Y],
displayRAc(T,Rest).

displayRAc([H|T],Rest):-
H\=["You", can, have,_, for, _],
displayRAc(T,Rest).
%---------------------------------
quit:-
ws(["Bye"]).


  chat(Q,QAc,RAc):-
  Q=[qit,'.'],
  displayAc(QAc,RAc),
  quit.
  
  
  chat(Q,QAc,RAc):-
  Q\=[qit,'.'],
  append(NQ,[_],Q),
  isValid(NQ),
  last(Q,W),
  W='?',
  response(NQ,QAc,RAc,R),
  ws(R),
  res(Q2),
  chat(Q2,[NQ|QAc],[R|RAc]).
  
  chat(Q,QAc,RAc):-
  Q\=[qit,'.'],
  append(NQ,[_],Q),
  \+isValid(NQ),
  ws(["I","can","not","understand","you"]),
  res(Q2),
  chat(Q2,QAc,RAc).
  
  chat(Q,QAc,RAc):-
  Q\=[qit,'.'],
  append(NQ,[_],Q),
  isValid(NQ),
  last(Q,W),
  W='.',
  R=["Ok"],
  ws(R),
  res(Q2),
  chat(Q2,[NQ|QAc],[R|RAc]).
  
  
  

  readInputTillQuit:-
  writeln('Welcome to your personal assistant'),
  res(Q),
  chat(Q,[],[]).
  %--------------------------------