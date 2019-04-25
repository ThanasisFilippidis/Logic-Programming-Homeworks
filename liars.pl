findAces([],Counter,Counter).
findAces([1|Tail],TmpCounter,Counter):-%find how many aces are in a list and return the number in counter
	TmpCounter1 is TmpCounter + 1,
	findAces(Tail,TmpCounter1,Counter).
findAces([0|Tail],TmpCounter,Counter):-
	findAces(Tail,TmpCounter,Counter).

mandatoryTrue([],_,_,_).
mandatoryTrue([Head|Tail],True,NumTrue,ListLength):-%check if the person saying the number of liars True is mandatory lying or could say true 
	Head =< True,	%check all the list of persons and if a person says less liars than the True consider him not liar as well
	NumTrue1 is NumTrue + 1, %and add one more true teller
	NumTrue1 =< (ListLength - True), %and check if the true tellers have become more than the people left after removing the liars
	mandatoryTrue(Tail,True,NumTrue1,ListLength).
mandatoryTrue([Head|Tail],True,NumTrue,ListLength):- %case for someone saying more liars exist than the True so we consider him as liar
	Head > True,
	mandatoryTrue(Tail,True,NumTrue,ListLength).

mandatoryTrue1([],_,[]).
mandatoryTrue1([Head|Tail],L,[0|Liars]):-%keeping starting list L with all the statements and iterating through [Head|Tail] to examine every person
	length(L,Len),
	mandatoryTrue(L,Head,0,Len),	%if he is able of telling the truth we keep him as true teller
	mandatoryTrue1(Tail,L,Liars).
mandatoryTrue1([Head|Tail],L,[1|Liars]):-
	length(L,Len),
	not(mandatoryTrue(L,Head,0,Len)),	%if he is unable to tell the truth he is kept as liar
	mandatoryTrue1(Tail,L,Liars).

verify([],[],_).
verify([Head|Tail],[1|Tail2],Counter):-	%predicate to check if the liars list we made is actually represeinting the truth
	Head > Counter,	%it checks that where ever we declared a liar he was saying that more liars exist than they really do
	verify(Tail,Tail2,Counter).
verify([Head|Tail],[0|Tail2],Counter):-	%because if he was sayin equal or less he was saying the truth
	Head =< Counter,
	verify(Tail,Tail2,Counter).

liars(L,Liars):-
	mandatoryTrue1(L,L,Liars), %so we make our list of liars in the start
	findAces(Liars,0,Counter), %we find how many are the liars
	verify(L,Liars,Counter). %and we verify that number