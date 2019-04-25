sameCharList((_,0),[]).	%taking as input a tuple it "flattens" it
sameCharList((Char,Times),[Char|Tail]):-
	Times > 0,
	Times1 is Times - 1,
	sameCharList((Char,Times1),Tail).

decode_rl1([],LFinal,LFinal).
decode_rl1([(Char,Times)|Tail],LStart,LFinal):-	%if the head of the list is tuple
	sameCharList((Char,Times),LTemp),	%flatten it
	append(LStart,LTemp,LTemp1),	%append the new flattened tuple to the others already flattened
	decode_rl1(Tail,LTemp1,LFinal),
	!.
decode_rl1([Head|Tail],LStart,LFinal):-	%if the head of the list is character
	append(LStart,[Head],LTemp1),
	decode_rl1(Tail,LTemp1,LFinal).

decode_rl(StartList, L):-
	decode_rl1(StartList,[],L).


encode([X], [X|TmpList], [X]):-	%if there is only one character left
	length([X|TmpList],1).	%and is alone in tmp list (so the ones before were different)
encode([X], [X|TmpList], [(X,Num)]):-%if there is only one character left
	length([X|TmpList],Num),
	Num > 1. %and in the  list are more than one characters (so the ones before are the same)
encode([X,X|Tail], TmpList, L):-%if the current character is the same as the one after
	encode([X|Tail], [X|TmpList], L).%continue constructing a tuple of them at TmpList
encode([X,Y|Tail], TmpList, [X|L]):-%if they are different and it was different from the one before too (so the tmpList length is 1) add him as single char
	not(X = Y),
	length(TmpList,1),
	encode([Y|Tail], [Y], L).
encode([X,Y|Tail], TmpList, [(X,CharsNum)|L]):-%if they are different and its the same from the one before(so the tmpList length is > 1) add him as tuple
	length(TmpList,ListLength),
	ListLength > 1,
	not(X = Y),
	length(TmpList,CharsNum),
	encode([Y|Tail], [Y], L).

encode_rl([],[]).
encode_rl([Head|Tail],L):-
	encode([Head|Tail], [Head], L).

% The answer given on the extra question is L = [(p(3), 2), (q(3), 2), q(4)] because since inside every "struct" there is a variable the predicate
% unifies in tuples all the same predicates and every variable takes a value the first time it is unified and then that value remains the same wherever
% this variable is used even in other predicates