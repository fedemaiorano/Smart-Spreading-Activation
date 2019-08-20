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

%Starting from SourceNodes, execute 1 iterations of the algorithm. F is firing treshold parameter, D the decay factor, Result the resulting activation table.
spreading_activation(SourceNodes,F,D,1,Result):-
	nodes(Nodes),
	activation_tab(Nodes,ActTab),
	spreading_activation_iter(F,D,SourceNodes,ActTab,Result),
	!.

%Starting from SourceNodes, execute NIteration iterations of the algorithm. F is firing treshold parameter, D the decay factor, Result the resulting activation table.
spreading_activation(SourceNodes,F,D,NIteration,Result):-
	N is NIteration-1,
	spreading_activation(SourceNodes,F,D,N,R),
	spreading_activation_iter(F,D,[],R,Result).

%With an input activation table.
spreading_activation(SourceNodes,F,D,1,Input,Result):-
	spreading_activation_iter(F,D,SourceNodes,Input,Result),
	!.

spreading_activation(SourceNodes,F,D,NIteration,Input,Result):-
	N is NIteration-1,
	spreading_activation(SourceNodes,F,D,N,Input,R),
	spreading_activation_iter(F,D,[],R,Result).

%The iteration of the algorithm.
spreading_activation_iter(F,D,[],Tab,Res):-
	get_firing_nodes(Tab,F,FNodes),
	check_spreading_edges(FNodes,D,Tab,NewTab),
	set_fired(FNodes,NewTab,Res),
	!.

spreading_activation_iter(F,D,SNodes,ActTab,Res):-
	set_activation(SNodes,1,ActTab,Tab),
	get_firing_nodes(Tab,F,FNodes),
	check_spreading_edges(FNodes,D,Tab,NewTab),
	set_fired(FNodes,NewTab,Res).

%Get the list of firing nodes (not already fired and with activation greater than F, firing treshold).
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

%Check edges starting from firing nodes. Change activation value of target unfired nodes.
%For each edges (X,Y) or (Y,X), if Y is not fired, Y's activation = Y's activation + X's activation * decay factor D * weight of (X,Y)/(Y,X).
check_spreading_edges([],_,In,In):-!.
check_spreading_edges([H|T],D,In,Out):-
	edges(EL),
	adjacents(H,EL,Adj),
	activate_adjacents(H,Adj,D,In,Out1),
	check_spreading_edges(T,D,Out1,Out).
	
activate_adjacents(_,[],_,In,In):-!.	
activate_adjacents(N,[H|T],D,In,Out):-
	get_fired(H,In,F),
	F == 0
	-> 
	get_activation(H,In,ActH),
	get_activation(N,In,ActN),
	%edges(EL),
	%get_weight((N,H),EL,W),
	Value is ActH + (ActN * D * 1), 
	round(Value,3,V),
	((V > 1 ; V < 0)
	->
	normalize(V,V1)
	;
	V1 is V),
	set_activation([H],V1,In,Out1),
	activate_adjacents(N,T,D,Out1,Out)
	;
	activate_adjacents(N,T,D,In,Out).
	
%Create the activation list. Elements are in the form (Node,Value,Fired),
%where Node is a node of the graph, Value is its activation value, and Fired is 1 if the node 
%is activated, or 0 if the node isn't already activated.
activation_tab([],[]).
activation_tab([[H|_]|T],[(H,0,0)|T1]):-
	activation_tab(T,T1).
	
%Update Input list, setting activation of nodes to Value, return updated list Res.
set_activation([],_,Input,Input).
set_activation([H|T],Value,Input,Res):-
	updateAct(H,Value,Input,R1),
	set_activation(T,Value,R1,Res).
	
updateAct(_,_,[],[]):-!.
updateAct(Node,Value,[(Node,_,F)|T],[(Node,Value,F)|T]):-
	!.
updateAct(Node,Value,[H|T],[H|T1]):-
	updateAct(Node,Value,T,T1).
	
%Update Input list, setting Fired value to 1, return updated list Res.
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

%Get the activation tab sorted by activation value.
ranking(ActTab,Sorted):-
	sort(2,@>=,ActTab,Sorted).

%get the activation value of a node.
get_activation(N,[_|T],A):-
	get_activation(N,T,A),
	!.
get_activation(N,[(N,A,_)|_],A).

%get the fired status of a node.
get_fired(N,[_|T],F):-
	get_fired(N,T,F),
	!.
get_fired(N,[(N,_,F)|_],F).

%get the weight of an edge.
get_weight((X,Y),[_|T],W):-
	get_weight((X,Y),T,W),
	!.
get_weight((X,Y),[(X,Y,W)|_],W):-!.
get_weight((X,Y),[(Y,X,W)|_],W):-!.
	
%Get the adjacents of a node.  
adjacents(_,[],[]):-!.
adjacents(N,[H|T],[Y|T1]):-
	contains(N,H,Y),
	!,
	adjacents(N,T,T1).
adjacents(N,[_|T],T1):-
	adjacents(N,T,T1).
  	
%Check if edge contains node X, return the adjacent node.
contains(X,[X,Y|_],Y).
contains(X,[Y,X|_],Y):-!.

%Put an element into a list, only if element is not [].
put(L,[],L):-!.
put([],E,[E]):-!.
put([H|T],E,[L1,H|L]):-
	put(T,E,[L1|L]).

%Round X float number to D decimal digit.
round(X,D,Res):- 
	Y is X * 10^D, 
	round(Y, Z), 
	Res is Z/10^D.
