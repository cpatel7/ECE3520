/* Chintan Patel */
/* ECE3520 SDE3  */

/*
	3.1 distanceR2/3

	Prototype:
	distanceR2(+V1,+V2,-Dsq)
	Arguments: vectors V1 and V2 (as lists), Dsq is result
	Notes:
	1. A necessary capability is, given a vector, to be able to find the closest
	vector in another set of vectors.
	The distance between any two vectors is computed by forming the difference vector
	and then taking the square root of the inner product of this difference vector
	with itself. It is also the square root of the sum of the squared elements of the
	difference vector. However, since we are interested in minimum distances
	(which correspond to minima of distances squared), we leave out the square root computation.
	2. This may be done recursively, element-by-element.

*/

distanceR2([HA|TA],[HB|TB], Result) :- 
	distance_squared(TA, TB, (HA-HB)**2,Dsq),
	Result is Dsq.

distance_squared([],[],H, H).
distance_squared([HA|TA],[HB|TB], H0, H+H0) :-
	distance_squared(TA, TB, (HA-HB)**2, H).	

/*
	3.2 distanceSqAllMeans/3

	Prototype:
	distanceSqAllMeans(+V,+Vset,-Dsq)
	Arguments: a vector and a set of vectors (represented as lists), and a vector of the
	distances from v to each element of vset.
	Notes: The objective is to take a single vector, V, and a set of vectors (to be the
	current means in list-of-lists form) and compute the distance squared from V to each
	of the vectors in the given set. The result is a list of squared distances.oiu
*/	

distanceSqAllMeans(V1,L1,Dsq):-  
	distanceSqAllMeansRec(V1,L1,[],LR),
	reverse(LR,Dsq).
	
distanceSqAllMeansRec(_,[],A,A):- !.	
distanceSqAllMeansRec(V1,[H|T],A,Dsq):-
	distanceR2(V1,H,D),
	distanceSqAllMeansRec(V1,T,[D|A],Dsq).
	
/*		
	3.3 listMinPos/2
	
	Prototype: listMinPos(+Alist,-M)
	Arguments: Alist, M is position (0-based indexing) of the minimum in the list
*/

listMinPos(L1,M):-
	min_list(L1,N),
	listMinPosRec(L1,N,M).

listMinPosRec([N|_],N,0):- !.
listMinPosRec([_|T],N,P):- 
	listMinPosRec(T,N,I),
	!,
	P is (I+1).

/*
	3.4 elsum/3
	
	Prototype: elsum(+L1,+L2,-S)
	Arguments: L1,L2 and S are lists of same length
	Notes:
	1. Implement vector addition as list ’addition’ of
	element by element sums of lists L1 and L2
	2. Add corresponding elements recursively
	3. DO NOT NEED LIST LENGTH.
*/

elsum([],[],[]):- !.
elsum([H1|T1],[H2|T2],[HS|TS]):-
	elsum(T1,T2,TS),
	!,
	HS is H1+H2.

/*
	3.5 scaleList/3
	
	Prototype: scaleList(+List,+Scale,-Answer)
	Arguments: List, Scale factor, Answer is List with each element divided by scale factor.
	Notes:
	1. Simple utility for use in forming next set of means.
	2. Must handle empty lists and division by zero (see examples).
*/

scaleList([],_,[]):- !.
scaleList(L1,0,L1):- !.
scaleList([H1|T1],Scale,[HA|TA]):-
	scaleList(T1,Scale,TA),
	!,
	HA is H1/Scale.

/*
	3.6 zeroes/2

	Prototype: zeroes(+Size,-TheList)
	Notes:
	creates a list of zeroes (0.0) of length
*/

zeroes(S,L):-
	createZeroes(0.0,S,L).
	
createZeroes(_,0,[]).	
createZeroes(Z,C,[Z|L]):-
	C>0, C1 is C-1, createZeroes(Z,C1,L), !.

/*
	3.7 zeroMeansSet/3
	Prototype: zeroMeansSet(+Cmeans,+Dim,-Set)
	Note creates a list (Set) of Cmeans lists, each all zeros with length Dim
*/

zeroMeansSet(0,_,[]).
zeroMeansSet(NL,NZ,[Z|L]):-
	NL>0, N1 is NL-1,
	zeroes(NZ,Z),
	zeroMeansSet(N1,NZ,L), !.

/*
	3.8 zeroVdiff/2
	This predicate tests vector equivalence.
	Prototype: zeroVdiff(+V1,+V2)
	Succeeds if V1 and V2 are identical.
*/

zeroVdiff(L1,L2):- L1 = L2.
	
/*
	3.9 zeroSetDiff/2
	Here we test if two sets of means (2 list of lists) are equal. This is a termi-
	nation condition.
	Prototype: zeroSetDiff(+S1,+S2)
	Arguments: 2 list-of-lists S1 and S2
	Succeeds (true) if S1 and S2 are equal; false otherwise
*/

zeroSetDiff(S1,S2):- S1 = S2.

/*
	3.10 zeroCounts/2
	Prototype: zeroCounts(+C,-CountsList)
	like predicate zeroes, but creates integer values
*/

zeroCounts(S,L):-
	createZeroCounts(0,S,L).
	
createZeroCounts(_,0,[]).	
createZeroCounts(Z,C,[Z|L]):-
	C>0, C1 is C-1, createZeroes(Z,C1,L), !.
	
/*
	3.11 updateCounts/3
	Prototype: updateCounts(+P,+Counts,-Updated)
	Arguments: Updated Counts list with element P incremented by 1
	Notes:
	Predicate to keep track of # of elements in a cluster.
	Records # of vectors closest to mean P as an integer.
	For eventual use in computing new cluster mean.
	Easy to see from examples.
	Reminder: 0-based indexing
*/

updateCounts(P,Counts,Update):-
	nth0(P,Counts,X),
	X1 is X+1,
	updateCountsRec(X1,P,Counts,Update).

updateCountsRec(X,0,[_|T],[X|T]):- !.	
updateCountsRec(X,P,[HC|TC],[HC|TU]):-	
	P > -1, NP is P-1,
	updateCountsRec(X,NP,TC,TU), !.
	
/*
	4.1 updateMeansSum/4

	Prototype: updateMeansSum(+V,+X,+Means,-NewMeansSum)
	Add a vector, V, to a vector at index X in a set of vectors (Means)
	with the result in NewMeansSum.
	Notes: It is not necessary to explicitly form the new cluster sets prior to forming
	the new means. In fact, this is inefficient.
	Instead, we simply keep a running sum of the vectors summed in a cluster
	and the number of vectors in the cluster. This predicate is a key part of cmeans computation.
*/	
	
updateMeansSum(V,X,Means,NewMeansSum):-
	nth0(X,Means,L1),
	elsum(L1,V,S),
	updateMeansSumRec(S,X,Means,NewMeansSum).

updateMeansSumRec(V,0,[_|T],[V|T]):- !.
updateMeansSumRec(V,I,[HM|TM],[HM|TN]):-
	I > -1, NI is I-1,
	updateMeansSumRec(V,NI,TM,TN), !.
	
/*
	4.2 formNewMeans/3

	Prototype: formNewMeans(+Newmeanssum, +Newcounts,-NewMeans)
	Recomputation of the means; this predicate uses Newmeanssum and Newcounts to form NewMeans.
	Really just normalization of Newmeanssum for each class by dividing each vector
	by the number of vectors in the cluster.
	Now you should see the utility of the counting and summing as we classify.
*/

formNewMeans([],[],[]).
formNewMeans([HN|TN], [HC|TC], [HM|TM]):-
	scaleList(HN,HC,HM),
	formNewMeans(TN,TC,TM).
/*
	4.3 reclassify/3
	
	This predicate implements the heart of the algorithm.
	Prototype: reclassify(+H, +Currmeans, -UpdatedMeans)
	Arguments:
	1. H (used recursively, processing a single vector starting at head)
	2. Currmeans is current set of c means (required, of course, to allow reclassification of H)
	3. UpdatedMeans is ’new’ means set.
	Notes:
	0. The strategy is for this predicate to recursively reclassify each element of H.
	(using previously developed functions updateCounts and updateMeans).
	1. Other previously developed predicates (including formNewMeans) are used.
	An auxiliary predicate recommended (see note 3).
	2. We can determine c and vector dimension from Currmeans for count initialization.
	3. Notice also reclassify does not explicitly have arguments for Newmeanssum, Newcounts which a
	necessary to reclassify. These are initialized with zeroes.
*/	
	
reclassify(H,Currmeans,UpdatedMeans):-
	length(Currmeans, C),
	zeroCounts(C, CountsList),
	nth0(0,Currmeans,L1),
	length(L1, Dim),
	zeroMeansSet(C, Dim, NewSums),
	reclassifyH(H,Currmeans,CountsList,NewSums,UpdatedMeans).
	
reclassifyH([],_,Newcounts,Newmeanssum,NewMeans):- formNewMeans(Newmeanssum,Newcounts,NewMeans).	
reclassifyH([H|T],Currmeans,CountsList,NewSums,UpdatedMeans):-
	distanceSqAllMeans(H,Currmeans,Dsq),
	listMinPos(Dsq,P),
	updateCounts(P,CountsList,UpdatedCounts),
	updateMeansSum(H,P,NewSums,UpdatedSums),
	reclassifyH(T,Currmeans,UpdatedCounts,UpdatedSums,UpdatedMeans).
	
/*
	4.4 cmeans/3
	The final, top-level predicate, cmeans. This predicate should work for any
	c (< |H|) and any dimension of input vectors. Part of this function design
	relies on determining when to stop. Recall we adopted the philosophy: When
	done (solution achieved) all elements of all vectors in the list of means are
	unchanging.
	Prototype: cmeans(+H,+MuCurrent,-MuFinal)
	MuCurrent starts as muzero; c is derivable from muzero
	Stops when means not changing.
*/

cmeans(H,MuCurrent,MuFinal):-
	reclassify(H,MuCurrent,MuTemp),
	cmeansRec(H,MuCurrent,MuTemp,MuFinal), !.
	
cmeansRec(_,MuCurrent,MuTemp,MuFinal):- 
	zeroSetDiff(MuCurrent,MuTemp), 
	copyMeans(MuCurrent,MuFinal), !.	
cmeansRec(H,_,MuCurrent,MuFinal):-
	reclassify(H,MuCurrent,MuTemp),
	cmeansRec(H,MuCurrent,MuTemp,MuFinal), !.	

/* helper function to copy list L1 to list L2 */
	
copyMeans([],[]).
copyMeans([H|T1],[H|T2]):-
	copyMeans(T1,T2).
	
	
