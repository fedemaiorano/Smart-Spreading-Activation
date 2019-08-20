:- use_module(library(apply)).
:- use_module(library(csv)).

%Read from two different file, the list of nodes and the list of edges. Skips headers.
prepare_db(FileNodes,FileEdges):-
	csv_read_file(FileNodes, [_|N], []),
	rows_to_lists(N, Nodes),
	assertz(nodes(Nodes)),
	csv_read_file(FileEdges, [_|E], []),
	rows_to_lists(E, Edges),
	assertz(edges(Edges)).
  
rows_to_lists(Rows, Lists):-
	maplist(row_to_list, Rows, Lists).
  
row_to_list(Row, List):-
	Row =.. [row|List].
  
%Starting from SourceNodes, execute 1 iterations of the algorithm. 
%F is firing treshold parameter, Result the resulting activation table.
spreading_activation(SourceNodes,F,1,Result):-
	nodes(Nodes),
	activation_tab(Nodes,ActTab),
	spreading_activation_iter(F,SourceNodes,1,ActTab,Result),
	!.

%Starting from SourceNodes, execute NIteration iterations of the algorithm. 
%F is firing treshold parameter, Result the resulting activation table.
spreading_activation(SourceNodes,F,NIteration,Result):-
	N is NIteration-1,
	spreading_activation(SourceNodes,F,N,R),
	spreading_activation_iter(F,SourceNodes,0,R,Result).

%The same as above, but with an input activation table
spreading_activation(SourceNodes,F,1,Input,Result):-
	spreading_activation_iter(F,SourceNodes,1,Input,Result),
	!.

spreading_activation(SourceNodes,F,NIteration,Input,Result):-
	N is NIteration-1,
	spreading_activation(SourceNodes,F,N,Input,R),
	spreading_activation_iter(F,SourceNodes,1,R,Result).

%The iteration of the algorithm
spreading_activation_iter(F,SNodes,0,Tab,Res1):-
	get_firing_nodes(Tab,F,FNodes),
	check_spreading_edges(SNodes,FNodes,Tab,NewTab),
	set_fired(FNodes,NewTab,Res1),
	!.

spreading_activation_iter(F,SNodes,1,ActTab,Res1):-
	set_activation(SNodes,1,ActTab,Tab),
	get_firing_nodes(Tab,F,FNodes),
	check_spreading_edges(SNodes,FNodes,Tab,NewTab),
	set_fired(FNodes,NewTab,Res1).

%Get the list of firing nodes (not already fired 
%and with activation greater than F, the firing treshold).
get_firing_nodes([],_,[]):-!.
get_firing_nodes([H|T],F,Res):-
	check_node(H,F,N),
	get_firing_nodes(T,F,R1),
	put(R1,N,Res).
	
check_node((_,_,1),_,[]):-!.
check_node((Node,Activation,0),F,Res):-
	Activation >= F 
	-> Res=Node
	; Res=[].

%Check edges starting from firing nodes. 
%Change activation value of target unfired nodes.
%For each edges (X,Y) or (Y,X), if Y is not fired, 
%Y's activation = Y's activation + X's activation * I smart factor.
check_spreading_edges(_,[],In,In):-!.
check_spreading_edges(SN,[H|T],In,Out):-
	nodes(NL),
	edges(EL),
	adjacents(H,EL,Adj),
	activate_adjacents(SN,H,Adj,NL,EL,In,Out1),
	check_spreading_edges(SN,T,Out1,Out).
	
activate_adjacents(_,_,[],_,_,In,In):-!.	
activate_adjacents(SN,N,[H|T],NL,EL,In,Out):-
	get_fired(H,In,F),
	F == 0
	-> 
	get_activation(H,In,ActH),
	get_activation(N,In,ActN),
	%Computing the I factor
	compute_I_factor(SN,N,H,NL,EL,(I,L)),
	(L == 'weight'
	->
	Value is I,
	round(Value,3,V)
	;
	Value is ActH + (ActN * I),
	round(Value,3,V)),
	((V > 1 ; V < 0)
	->
	normalize(V,V1)
	;
	V1 is V),
	set_activation([H],V1,In,Out1),
	activate_adjacents(SN,N,T,NL,EL,Out1,Out)
	;
	activate_adjacents(SN,N,T,NL,EL,In,Out).
	
%Create the activation list. Elements are in the form (Node,Value,Fired),
%where Node is a node of the graph, Value is its activation value, 
%and Fired is 1 if the node is activated, or 0 if the node 
%isn't already activated.
activation_tab([],[]).
activation_tab([[H|_]|T],[(H,0,0)|T1]):-
	activation_tab(T,T1).
	
%Update Input list, setting activation of nodes to Value, returns updated list Res.
set_activation([],_,Input,Input).
set_activation([H|T],Value,Input,Res):-
	updateAct(H,Value,Input,R1),
	set_activation(T,Value,R1,Res).
	
updateAct(_,_,[],[]):-!.
updateAct(Node,Value,[(Node,_,F)|T],[(Node,Value,F)|T]):-
	!.
updateAct(Node,Value,[H|T],[H|T1]):-
	updateAct(Node,Value,T,T1).
	
%Update Input list, setting Fired value to 1, returns updated list Res.
set_fired([],Input,Input).
set_fired([H|T],Input,Res):-
	updateFir(H,Input,R1),
	set_fired(T,R1,Res).
	
updateFir(_,[],[]):-!.
updateFir(Node,[(Node,A,_)|T],[(Node,A,1)|T]):-
	!.
updateFir(Node,[H|T],[H|T1]):-
	updateFir(Node,T,T1).	

%Set value > 1 to 1, and value < 0 to 0.
normalize(V,1):-
	V > 1,
	!.
normalize(V,0):-
	V < 0.
		
%Get the activation tab sorted by activation value
ranking(ActTab,Sorted):-
	sort(2,@>=,ActTab,Sorted).

%Get the activation only of nodes of type Type
activation_type([],_,[]).
activation_type([(N,P,_)|T],Type,[(N,P)|T1]):-
	nodes(NL),
	get_type(N,NL,Type),
	%format('~w ~n',[(N,P)]),
	!,
	activation_type(T,Type,T1).
activation_type([_|T],Type,T1):-
	activation_type(T,Type,T1).

%Get the activation value of a node
get_activation(N,[_|T],A):-
	get_activation(N,T,A),
	!.
get_activation(N,[(N,A,_)|_],A).

%Get the fired status of a node
get_fired(N,[_|T],F):-
	get_fired(N,T,F),
	!.
get_fired(N,[(N,_,F)|_],F).

%Get the type of a node
get_type(Node,[[Node,Type|_]|_],Type):-!.
get_type(Node,[_|T],Type):-
	get_type(Node,T,Type),
	!.

%Get the label of an edge
get_label(N1,N2,[[N1,N2,L|_]|_],L):-!.
get_label(N1,N2,[[N2,N1,L|_]|_],L):-!.
get_label(N1,N2,[_|T],L):-
	get_label(N1,N2,T,L).

%Get the adjacents of a node  
adjacents(_,[],[]):-!.
adjacents(N,[H|T],[Y|T1]):-
	contains(N,H,Y),
	!,
	adjacents(N,T,T1).
adjacents(N,[_|T],T1):-
	adjacents(N,T,T1).
  	
%Check if edge contains node X, return the adjacent node
contains(X,[X,Y|_],Y).
contains(X,[Y,X|_],Y):-!.

%Put an element into a list, only if element is not []
put(L,[],L):-!.
put([],E,[E]):-!.
put([H|T],E,[L1,H|L]):-
	put(T,E,[L1|L]).

%Round X float number to D decimal digit
round(X,D,Res):- 
	Y is X * 10^D, 
	round(Y, Z), 
	Res is Z/10^D.

%Get the Nth element of a list. First element of the list is at 1.
get([H|_],1,H):-!.
get([_|T],N,R):-
	N1 is N-1,
	get(T,N1,R).
	
%Computing the I factor
compute_I_factor([],_,_,_,_,(0,_)):-!.
compute_I_factor([H|T],N1,N2,NL,EL,(I,Res)):-
	get_label(N1,N2,EL,Label),
	((ignore(N1,Label,N2) ; ignore(N2,Label,N1))
	->
	I is 0,
	Res = 'ignore'
	;
	(weight(N1,Label,N2,W) ; weight(N2,Label,N1,W))
	->
	I is W,
	Res = 'weight'
	;
	findall(Int,interest(H,Int),L),
	get_type(N2,NL,Type),
	get_interest_type(L,Type,NL,L1),
	compute_I_interest(H,L1,N2,N1,NL,EL,I1),
	compute_I_factor(T,N1,N2,NL,EL,(I2,Res)),
	I is I1 + I2).
	
%Get interest list by Type
get_interest_type([],_,_,[]).
get_interest_type([H|T],Type,NL,[H|T1]):-
	get_type(H,NL,Type),
	!,
	get_interest_type(T,Type,NL,T1).
get_interest_type([_|T],Type,NL,T1):-
	get_interest_type(T,Type,NL,T1).
	
%Compute I by interest of SN
compute_I_interest(_,[],_,_,_,_,0):-!.	
compute_I_interest(SN,[H|T],N,N1,NL,EL,I):-
	compute_I_interest(SN,T,N,N1,NL,EL,I3),
	(interest(SN,N)
	->
	I is 1
	;
	n_columns(NCol),
	K is NCol-2,
	compute_I_attr(N,H,K,NL,I1),
	compute_I_adjs(N,H,EL,N1,I2),
	!,
	I is I1 + I2 + I3).

%Compute I comparing attributes of nodes
compute_I_attr(_,_,0,_,0):-!.
compute_I_attr(N,Int,K,NL,I):-
	K1 is K-1,
	compute_I_attr(N,Int,K1,NL,I2),
	get_attr_node(N,K,NL,AttrN),
	get_attr_node(Int,K,NL,AttrInt),
	((AttrN == AttrInt,AttrN \= '-')
	->
	Count is 1
	;
	Count is 0),
	I is Count*(0.1) + I2.

%Get the K'th attribute of Node
get_attr_node(Node,K,[[Node,_|Attr]|_],El):-
	get(Attr,K,El),
	!.
get_attr_node(Node,K,[_|T],Attr):-
	get_attr_node(Node,K,T,Attr).

%Compute I comparing adjacents of nodes
compute_I_adjs(N,H,EL,N1,I):-
	adjacents(N,EL,Adj1),
	adjacents(H,EL,Adj2),
	delete(Adj2,N1,AdjN),
	delete(Adj1,N1,AdjH),
	get_adjs_label(H,AdjN,EL,AdjLabelN),
	get_adjs_label(N,AdjH,EL,AdjLabelH),
	intersection(AdjLabelN,AdjLabelH,Ins),
	length(Ins,Count),
	I is Count*(0.25).

%Get the label of each adjacent edge
get_adjs_label(_,[],_,[]):-!.
get_adjs_label(N,[H|T],EL,[Res|Tl]):-
	get_label(N,H,EL,L),
	atom_concat(H,L,Res),
	get_adjs_label(N,T,EL,Tl).

%Number of columns in nodes.csv file
n_columns(27).
%KNOWNLEDGE BASE
interest(1102,831).
weight(1102,'wasIn',6,1).
weight(791,'wasIn',6,1).
weight(804,'wasIn',791,1).
ignore('','','').
