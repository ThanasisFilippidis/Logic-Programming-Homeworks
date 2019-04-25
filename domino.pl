dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
(2,2),(2,3),(2,4),(2,5),(2,6),
(3,3),(3,4),(3,5),(3,6),
(4,4),(4,5),(4,6),
(5,5),(5,6),
(6,6)]).

frame([[3,1,2,6,6,1,2,2],
[3,4,1,5,3,0,3,6],
[5,6,6,1,2,4,5,0],
[5,6,4,1,3,3,0,0],
[6,1,0,6,3,2,4,0],
[4,1,5,2,4,3,5,5],
[4,1,0,2,4,5,2,0]]).


fitsOne((Dom1, Dom2),Frame1, Frame2):-
	Dom1 =\= Dom2,
	Dom1 =:= Frame1,
	Dom2 =:= Frame2.
fitsOne((Dom1, Dom2),Frame1, Frame2):-
	Dom1 =\= Dom2,
	Dom1 =:= Frame2,
	Dom2 =:= Frame1.
fitsOne((Dom1, Dom2),Frame1, Frame2):-
	Dom1 =:= Dom2,
	Dom1 =:= Frame2,
	Dom2 =:= Frame1.

fitsHorizontalRow([_|[]],_,_,[]).
fitsHorizontalRow([RowHead1,RowHead2|RowTail],Dom,CurrCounter,[CurrCounter|HorizPos]):-
	fitsOne(Dom,RowHead1,RowHead2),
	CurrCounter1 is CurrCounter + 1,
	fitsHorizontalRow([RowHead2|RowTail],Dom,CurrCounter1,HorizPos).
fitsHorizontalRow([RowHead1,RowHead2|RowTail],Dom,CurrCounter,HorizPos):-
	\+fitsOne(Dom,RowHead1,RowHead2),
	CurrCounter1 is CurrCounter + 1,
	fitsHorizontalRow([RowHead2|RowTail],Dom,CurrCounter1,HorizPos).

fitsHorizontalFrame([],_,_,Pos,Pos).
fitsHorizontalFrame([Row|RestRows],Dom,CurrCounter,CurrPos,Pos):-
	fitsHorizontalRow(Row,Dom,CurrCounter,HorizPos),
	append(CurrPos,HorizPos,CurrPos1),
	length(Row,RowLength),
	SetsCounter is RowLength - 1,
	CurrCounter1 is CurrCounter + SetsCounter,
	fitsHorizontalFrame(RestRows,Dom,CurrCounter1,CurrPos1,Pos).

fitsVerticalRow([],[],_,_,[]).
fitsVerticalRow([RowHead1|Row1],[RowHead2|Row2],Dom,CurrCounter,[CurrCounter|VertPos]):-
	fitsOne(Dom,RowHead1,RowHead2),
	CurrCounter1 is CurrCounter + 1,
	fitsVerticalRow(Row1,Row2,Dom,CurrCounter1,VertPos).
fitsVerticalRow([RowHead1|Row1],[RowHead2|Row2],Dom,CurrCounter,VertPos):-
	\+fitsOne(Dom,RowHead1,RowHead2),
	CurrCounter1 is CurrCounter + 1,
	fitsVerticalRow(Row1,Row2,Dom,CurrCounter1,VertPos).

fitsVerticalFrame([_|[]],_,_,Pos,Pos).
fitsVerticalFrame([Row1,Row2|RestRows],Dom,CurrCounter,CurrPos,Pos):-
	fitsVerticalRow(Row1,Row2,Dom,CurrCounter,VertPos),
	append(CurrPos,VertPos,CurrPos1),
	length(Row1,SetsCounter),
	CurrCounter1 is CurrCounter + SetsCounter,
	fitsVerticalFrame([Row2|RestRows],Dom,CurrCounter1,CurrPos1,Pos).

fitsFrame([Row|Frame],Dom,Pos):-
	fitsHorizontalFrame([Row|Frame],Dom,1,[],HorizPos),
	length([Row|Frame],RowNum),
	length(Row,RowLength),
	HorizontalSetsCounter is RowNum * (RowLength - 1),
	VertStartCounter is HorizontalSetsCounter + 1,
	fitsVerticalFrame([Row|Frame],Dom,VertStartCounter,[],VertPos),
	append(HorizPos,VertPos,Pos).

everyDomFit([],_,[]).
everyDomFit([Dom|RestDoms],Frame,[DomPos|AllPos]):-
	fitsFrame(Frame,Dom,DomPos),
	everyDomFit(RestDoms,Frame,AllPos).

posLengths([],[]).
posLengths([DomPos|AllPos],[PosLength|Tail]):-
	length(DomPos,PosLength),
	posLengths(AllPos,Tail).

findMin([PosLength|Tail], Min) :-
    findMin(Tail, PosLength, Min).

findMin([], Min, Min).
findMin([PosLength|Tail], CurrMin, Min) :-
    CurrMin1 is min(PosLength, CurrMin),
    findMin(Tail, CurrMin1, Min).

findDomMin([Dom|_],[DomPos|_],Min,Dom,DomPos):-
	length(DomPos,Min).
findDomMin([_|RestDoms],[DomPos|AllPos],Min,DomMin,DomMinPos):-
	length(DomPos,PosLength),
	PosLength > Min,
	findDomMin(RestDoms,AllPos,Min,DomMin,DomMinPos).

horFindDomRowColumn(Set,NumColumns,Row,Column):-
	HorSets is NumColumns - 1,
	Column is mod(Set,HorSets),
	Row is div(Set,HorSets).

horPosToDel(Set,NumRows,NumColumns,[P1,P2,P3,P4,P5,P6]):-
	horFindDomRowColumn(Set,NumColumns,Row,Column),
	Column =\= 0,
	HorSets is NumColumns - 1,
	Offset is HorSets * NumRows,
	P1 is Offset + (Row - 1) * NumColumns + Column,
	P2 is P1 + 1,
	P3 is Offset + Row * NumColumns + Column,
	P4 is P3 + 1,
	P5 is Set - 1,
	P6 is Set + 1.

horPosToDel(Set,NumRows,NumColumns,[P1,P2,P3,P4,P5]):-
	horFindDomRowColumn(Set,NumColumns,Row,Column),
	Column =:= 0,
	HorSets is NumColumns - 1,
	Offset is HorSets * NumRows,
	P1 is Offset + (Row - 1) * NumColumns + Column - 1,
	P2 is P1 + 1,
	P3 is Offset + Row * NumColumns + Column - 1,
	P4 is P3 + 1,
	P5 is Set - 1.

deleteSelectedIndexes([],_,_,[]).
deleteSelectedIndexes([Head|Tail],_,[],[Head|Tail]).
deleteSelectedIndexes([_|Tail],CurrIndex,[CurrIndex|RestIndexes],NewTail):-
	CurrIndex1 is CurrIndex + 1,
	deleteSelectedIndexes(Tail,CurrIndex1,RestIndexes,NewTail).
deleteSelectedIndexes([Head|Tail],CurrIndex,[Index|RestIndexes],[Head|NewTail]):-
	CurrIndex =\= Index,
	CurrIndex1 is CurrIndex + 1,
	deleteSelectedIndexes(Tail,CurrIndex1,[Index|RestIndexes],NewTail).

horRestrictionUp(Row,Column,NumRows,[1,2|RestrictionsList]):-
	Row =:= 0,
	Column =\= 0,
	horRestrictionDown(Row,Column,NumRows,RestrictionsList).

horRestrictionUp(Row,Column,NumRows,[1,2|RestrictionsList]):-
	Row =:= 1,
	Column =:= 0,
	horRestrictionDown(Row,Column,NumRows,RestrictionsList).

horRestrictionUp(Row,Column,NumRows,RestrictionsList):-
	Row > 0,
	horRestrictionDown(Row,Column,NumRows,RestrictionsList).

horRestrictionDown(Row,Column,NumRows,[3,4|RestrictionsList]):-
	Row =:= NumRows - 1,
	Column =\= 0,
	horRestrictionLeft(Row,Column,NumRows,RestrictionsList).

horRestrictionDown(Row,Column,NumRows,[3,4|RestrictionsList]):-
	Row =:= NumRows,
	Column =:= 0,
	horRestrictionLeft(Row,Column,NumRows,RestrictionsList).

horRestrictionDown(Row,Column,NumRows,RestrictionsList):-
	Row =:= NumRows - 1,
	Column =:= 0,
	horRestrictionLeft(Row,Column,NumRows,RestrictionsList).

horRestrictionDown(Row,Column,NumRows,RestrictionsList):-
	Row < NumRows - 1,
	horRestrictionLeft(Row,Column,NumRows,RestrictionsList).

horRestrictionLeft(_,Column,_,[5]):-
	Column =:= 1.

horRestrictionLeft(_,Column,_,[]):-
	Column =\= 1.

horFilter(Set,NumRows,NumColumns,PosToDel,FinalPosToDel):-
	horFindDomRowColumn(Set,NumColumns,Row,Column),
	horRestrictionUp(Row,Column,NumRows,RestrictionsList),
	deleteSelectedIndexes(PosToDel,1,RestrictionsList,FinalPosToDel).

horPosToDelFinal(Set,NumRows,NumColumns,FinalPosToDel):-
	horPosToDel(Set,NumRows,NumColumns,Pos),
	horFilter(Set,NumRows,NumColumns,Pos,FinalPosToDel).

verFindDomRowColumn(Set,NumRows,NumColumns,Row,Column):-
	RealSet is Set - (NumColumns - 1) * NumRows,
	Column is mod(RealSet,NumColumns),
	Row is div(RealSet,NumColumns).

verPosToDel(Set,NumRows,NumColumns,[P1,P2,P3,P4,P5,P6]):-
	verFindDomRowColumn(Set,NumRows,NumColumns,Row,Column),
	Column =\= 0,
	HorSets is NumColumns - 1,
	P1 is Set - NumColumns,
	P2 is Set + NumColumns,
	P3 is Row * HorSets + Column - 1,
	P4 is P3 + HorSets,
	P5 is P3 + 1,
	P6 is P4 + 1.

verPosToDel(Set,NumRows,NumColumns,[P1,P2,P3,P4]):-
	verFindDomRowColumn(Set,NumRows,NumColumns,Row,Column),
	Column =:= 0,
	HorSets is NumColumns - 1,
	P1 is Set - NumColumns,
	P2 is Set + NumColumns,
	P3 is Row * HorSets + Column,
	P4 is P3 + HorSets.

verRestrictionUp(Row,Column,NumRows,[1|RestrictionsList]):-
	Row =:= 0,
	Column =\= 0,
	verRestrictionDown(Row,Column,NumRows,RestrictionsList).

verRestrictionUp(Row,Column,NumRows,[1|RestrictionsList]):-
	Row =:= 1,
	Column =:= 0,
	verRestrictionDown(Row,Column,NumRows,RestrictionsList).

verRestrictionUp(Row,Column,NumRows,RestrictionsList):-
	Row > 0,
	verRestrictionDown(Row,Column,NumRows,RestrictionsList).

verRestrictionDown(Row,Column,NumRows,[2|RestrictionsList]):-
	Row =:= NumRows - 2,
	Column =\= 0,
	verRestrictionLeft(Row,Column,NumRows,RestrictionsList).

verRestrictionDown(Row,Column,NumRows,[2|RestrictionsList]):-
	Row =:= NumRows - 1,
	Column =:= 0,
	verRestrictionLeft(Row,Column,NumRows,RestrictionsList).

verRestrictionDown(Row,Column,NumRows,RestrictionsList):-
	Row =:= NumRows - 2,
	Column =:= 0,
	verRestrictionLeft(Row,Column,NumRows,RestrictionsList).

verRestrictionDown(Row,Column,NumRows,RestrictionsList):-
	Row < NumRows - 2,
	verRestrictionLeft(Row,Column,NumRows,RestrictionsList).

verRestrictionLeft(_,Column,_,[3,4]):-
	Column =:= 1.

verRestrictionLeft(_,Column,_,[]):-
	Column =\= 1.

verFilter(Set,NumRows,NumColumns,PosToDel,FinalPosToDel):-
	verFindDomRowColumn(Set,NumRows,NumColumns,Row,Column),
	verRestrictionUp(Row,Column,NumRows,RestrictionsList),
	deleteSelectedIndexes(PosToDel,1,RestrictionsList,FinalPosToDel).

verPosToDelFinal(Set,NumRows,NumColumns,FinalPosToDel):-
	verPosToDel(Set,NumRows,NumColumns,Pos),
	verFilter(Set,NumRows,NumColumns,Pos,FinalPosToDel).

deleteRestrFromDomPos([],_,[]).
deleteRestrFromDomPos([Head|RestPos],Restrictions,[Head|NewRestPos]):-
	\+ member(Head,Restrictions),
	deleteRestrFromDomPos(RestPos,Restrictions,NewRestPos).
deleteRestrFromDomPos([Head|RestPos],Restrictions,NewRestPos):-
	member(Head,Restrictions),
	deleteRestrFromDomPos(RestPos,Restrictions,NewRestPos).

deleteRestrFromAllDomPos([],_,[]).
deleteRestrFromAllDomPos([DomPos|AllPos],Restrictions,[NewDomPos|NewAllPos]):-
	deleteRestrFromDomPos(DomPos,Restrictions,NewDomPos),
	length(NewDomPos,PosLength),
	PosLength >= 1,
	deleteRestrFromAllDomPos(AllPos,Restrictions,NewAllPos).

deleteSelectedDomPos([Dom|RestDoms],[_|RestPos],Dom,RestDoms,RestPos).
deleteSelectedDomPos([Dom|RestDoms],[Pos|RestPos],MyDom,[Dom|MyRestDoms],[Pos|MyRestPos]):-
	deleteSelectedDomPos(RestDoms,RestPos,MyDom,MyRestDoms,MyRestPos).

selectPos(Pos,NumRows,NumColumns,SelectedPos,FinalPosToDel):-
	member(SelectedPos,Pos),
	Offset is (NumColumns - 1) * NumRows,
	SelectedPos > Offset,
	verPosToDelFinal(SelectedPos,NumRows,NumColumns,FinalPosToDel).
selectPos(Pos,NumRows,NumColumns,SelectedPos,FinalPosToDel):-
	member(SelectedPos,Pos),
	Offset is (NumColumns - 1) * NumRows,
	SelectedPos =< Offset,
	horPosToDelFinal(SelectedPos,NumRows,NumColumns,FinalPosToDel).

setPos([],[],_,_,[]).
setPos(Doms,Pos,NumRows,NumColumns,[SelectedPos|Tail]):-
	posLengths(Pos,PosLengths),
	findMin(PosLengths,Min),
	findDomMin(Doms,Pos,Min,DomMin,DomMinPos),
	selectPos(DomMinPos,NumRows,NumColumns,SelectedPos,FinalPosToDel),
	deleteSelectedDomPos(Doms,Pos,DomMin,NewDoms,NewPos),
	deleteRestrFromAllDomPos(NewPos,FinalPosToDel,FinalPos),
	setPos(NewDoms,FinalPos,NumRows,NumColumns,Tail).

printRow(_,_,[Head]):-
	write(Head),nl.
printRow(Element,Solution,[Head|Row]):-
	length(Row, RowLength),
	RowLength > 0,
	member(Element,Solution),
	write(Head),write('-'),
	Element1 is Element + 1,
	printRow(Element1,Solution,Row).
printRow(Element,Solution,[Head|Row]):-
	length(Row, RowLength),
	RowLength > 0,
	\+member(Element,Solution),
	write(Head),write(' '),
	Element1 is Element + 1,
	printRow(Element1,Solution,Row).

printColumn(Element,Solution,NumRows,NumColumn):-
	Column is mod((Element - (NumColumn - 1) * NumRows),NumColumn),
	Column =\= 0,
	member(Element,Solution),
	write('|'),write(' '),
	Element1 is Element + 1,
	printColumn(Element1,Solution,NumRows,NumColumn).
printColumn(Element,Solution,NumRows,NumColumn):-
	Column is mod((Element - (NumColumn - 1) * NumRows),NumColumn),
	Column =\= 0,
	\+member(Element,Solution),
	write(' '),write(' '),
	Element1 is Element + 1,
	printColumn(Element1,Solution,NumRows,NumColumn).
printColumn(Element,Solution,NumRows,NumColumn):-
	Column is mod((Element - (NumColumn - 1) * NumRows),NumColumn),
	Column =:= 0,
	member(Element,Solution),
	write('|'),nl.
printColumn(Element,Solution,NumRows,NumColumn):-
	Column is mod((Element - (NumColumn - 1) * NumRows),NumColumn),
	Column =:= 0,
	\+member(Element,Solution),
	write(' '),nl.

printSolution(RowElement,_,[Row],Solution,_,_):-
	printRow(RowElement,Solution,Row).
printSolution(RowElement,ColumnElement,[Row|Frame],Solution,NumRows,NumColumn):-
	length(Frame,FrameLength),
	FrameLength > 0,
	printRow(RowElement,Solution,Row),
	Offset is (NumColumn - 1) * NumRows,
	ActualElement is Offset + ColumnElement,
	printColumn(ActualElement,Solution,NumRows,NumColumn),
	RowElement1 is RowElement + (NumColumn - 1),
	ColumnElement1 is ColumnElement + NumColumn,
	printSolution(RowElement1,ColumnElement1,Frame,Solution,NumRows,NumColumn).

getData(Doms,[Row|Frame],Positions,NumRows,NumColumn):-
	everyDomFit(Doms,[Row|Frame],Positions),
	length(Row,NumColumn),
	length([Row|Frame],NumRows).

put_dominos:-
	dominos(Doms),
	frame(Frame),
	getData(Doms,Frame,Positions,NumRows,NumColumn),
	setPos(Doms,Positions,NumRows,NumColumn,Solution),
	printSolution(1,1,Frame,Solution,NumRows,NumColumn).

	


















