% Componenti del gruppo:

% Colombo Filippo 829745
% Magni Lorenzo 816114
% Manganaro Francesco 845087

:- dynamic heap/2.      
:- dynamic heap_entry/4.  
:- dynamic graph/1.   
:- dynamic vertex/2.  
:- dynamic arc/3.     
:- dynamic arc/4.
:- dynamic dist/3.      
:- dynamic visited/2.   
:- dynamic previous/3.  

% GRAFI PROLOG

new_graph(G) :- 
	graph(G),
	!.

new_graph(G) :- 
	assert(graph(G)),
	!.

delete_graph(G) :-
	graph(G),
	retractall(arc(G, _, _, _)),
	retractall(vertex(G, _)), 
	retractall(graph(G)),
	!.

new_vertex(G, V) :-
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	!.

new_vertex(G, V) :-
	assert(vertex(G, V)),
	!.

vertices(G, Vs) :-
	nonvar(G),
	graph(G),
	findall(V, vertex(G, V), Weight),
	Vs = Weight.

list_vertices(G) :-
	nonvar(G),
	graph(G),
	listing(vertex(G, _)).

new_arc(G, U, V) :- 
	new_arc(G, U, V, 1),
	!.

new_arc(G, U, V, Weight) :- 
	nonvar(G),
	nonvar(U),
	nonvar(V),
	nonvar(Weight),
	graph(G),
	arc(G, U, V, _),
	retractall(arc(G, U, V, _)),
	new_arc(G, U, V, Weight),
	!.

new_arc(G, U, V, Weight) :-
	assert(arc(G, U, V, Weight)),
	!.

arcs(G, Es) :-
	nonvar(G),
	graph(G),
	Arc = arc(G, _, _, _),
	findall(Arc, Arc, Arcs),
	Es = Arcs.

neighbors(G, V, Ns) :-
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	Neighbor = arc(G, V, _, _),
	findall(Neighbor, Neighbor, Neighbors),
	Ns = Neighbors.

list_arcs(G) :-
	nonvar(G),
	graph(G),
	listing(arc(G, _, _, _)).

list_graph(G) :-
	nonvar(G),
	graph(G),
	list_vertices(G),
	list_arcs(G).

% SSSP IN PROLOG

change_dist(G, V, NewDist) :-
	nonvar(G),
	nonvar(V),
	nonvar(NewDist),
	graph(G),
	vertex(G, V),
	retractall(dist(G, V, _)),
	assert(dist(G, V, NewDist)),
	!.

change_previous(G, V, U) :-
	nonvar(G),
	nonvar(V),
	nonvar(U),
	graph(G),
	vertex(G, V),
	vertex(G, U),
	retractall(previous(G, V, _)),
	assert(previous(G, V, U)),
	!.

sssp(G, Source) :-
	nonvar(G),
	nonvar(Source),
	graph(G),
	vertex(G, Source),
	new_heap(h),
	retractall(previous(G, _, _)),
	retractall(dist(G, _, _)),
	retractall(visited(G, _)),
	vertices(G, Vs),
	initialise_single_source(G, Source, h, Vs),
	change_dist(G, Source, 0),
	max_int(Max),
	modify_key(h, 0, Max, Source),
	dijkstra(G, Source, h),
	delete_heap(h),
	!.

shortest_path(G, Source, V, Path) :-
	nonvar(G),
	nonvar(Source),
	nonvar(V),
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	Source = V,
	Path = [],
	!.

shortest_path(G, Source, V, Path) :-
	nonvar(G),
	nonvar(Source),
	nonvar(V),
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	Source \= V,
	previous(G, V, U),
	arc(G, U, V, Weight),
	shortest_path(G, Source, U, Ptmp),
	append(Ptmp, [arc(G, U, V, Weight)], Path),
	!.

% MINHEAP IN PROLOG

new_heap(H) :- 
	heap(H, _),
	!.

new_heap(H) :-
	assert(heap(H, 0)),
	!.

delete_heap(H) :- 
	heap(H, 0),
	retract(heap(H, 0)),
	!.

delete_heap(H) :-
	heap(H, _),
	retractall(heap_entry(H, _, _, _)),
	retract(heap(H, _)),
	!.

heap_size(H, S) :-
	heap(H, S),
	!.

empty(H) :-   
	nonvar(H),
	heap(H, 0),
	!.

not_empty(H) :-   
	nonvar(H),
	heap(H, S),
	S > 0.

head(H, K, V) :- 
	nonvar(H),
	heap(H, _),
	heap_entry(H, 1, K, V),
	!.

insert(H, K, V) :-
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, _),
	not_empty(H),
	heap_entry(H, _, K, V),
	!.

insert(H, K, V) :-
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, _),
	not_empty(H),
	heap_entry(H, _, OldKey, V),
        modify_key(H, K, OldKey, V),
	!.

insert(H, K, V) :-
	nonvar(H),
	nonvar(K),
	nonvar(V),
	heap(H, S),
	Stmp is S + 1,
	\+ heap_entry(H, _, K, V),
	retract(heap(H, S)),
	assert(heap(H, Stmp)),
	assert(heap_entry(H, Stmp, K, V)),
	propetyHeap(H, Stmp),
	!.

extract(H, K, _) :-
	nonvar(H),
	nonvar(K),
	empty(H),
	!.

extract(H, K, V) :- 
	nonvar(H),
	nonvar(V),
	heap(H, _),
	not_empty(H),
	\+ head(H, K, V),
	!.

extract(H, K, V) :- 
	nonvar(H),
	nonvar(V),
	heap(H, S),
	not_empty(H),
	S = 1,
	heap_entry(H, S, K, V),
	Stmp is S - 1,
	retract(heap_entry(H, S, K, V)),
	retract(heap(H, S)),
	assert(heap(H, Stmp)),
	!.

extract(H, K, V) :-
	nonvar(H),
	nonvar(V),
	heap(H, S),
	not_empty(H),
	Stmp is S - 1,
	head(H, K, V),
	heap_entry(H, S, Klast, Vlast),
	scambio(H, 1, K, V, S, Klast, Vlast),
	retract(heap_entry(H, S, K, V)),
	retract(heap(H, S)),
	assert(heap(H, Stmp)),
	heapify(H, 1),
	!.

modify_key(H, NewKey, OldKey, V) :- 
	nonvar(H),
	nonvar(NewKey),
	nonvar(OldKey),
	nonvar(V),
	NewKey = OldKey,
	heap_entry(H, _, OldKey, V),
	!.

modify_key(H, NewKey, OldKey, V) :-
	nonvar(H),
	nonvar(NewKey),
	nonvar(OldKey),
	nonvar(V),
	heap_entry(H, P, OldKey, V),
	NewKey < OldKey,
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	propetyHeap(H, P),
	!.

modify_key(H, NewKey, OldKey, V) :- 
	nonvar(H),
	nonvar(NewKey),
	nonvar(OldKey),
	nonvar(V),
	heap_entry(H, P, OldKey, V),
	NewKey > OldKey,
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	heapify(H, P),
	!.

list_heap(H) :- 
	nonvar(H),
	listing(heap_entry(H, _, _, _)).

% FUNZIONI AGGIUNTIVE

scambio(H, P1, K1, V1, P2, K2, V2) :-
	nonvar(H),
	nonvar(P1),
	nonvar(K1),
	nonvar(V1),
	nonvar(P2),
	nonvar(K2),
	nonvar(V2),
	heap_entry(H, P1, K1, V1),
	heap_entry(H, P2, K2, V2),
	retract(heap_entry(H, P1, K1, V1)),
	retract(heap_entry(H, P2, K2, V2)),
	assert(heap_entry(H, P2, K1, V1)),
	assert(heap_entry(H, P1, K2, V2)),
	!.

minimum(H, Pone, Ptwo, Min) :- 
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	heap(H, S),
	S >= Pone,
	S >= Ptwo,
	heap_entry(H, Pone, Kone, _),
	heap_entry(H, Ptwo, Ktwo, _),
	Kone < Ktwo,
	Min is Pone,
	!.

minimum(H, Pone, Ptwo, Min) :- % Kone >= Ktwo
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	heap(H, S),
	S >= Pone,
	S >= Ptwo,
	heap_entry(H, Pone, Kone, _),
	heap_entry(H, Ptwo, Ktwo, _),
	Kone >= Ktwo,
	Min is Ptwo,
	!.

minimum(H, Pone, Ptwo, Pthree, Min) :-
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	nonvar(Pthree),
	heap(H, S),
	S >= Pone,
	S >= Ptwo,
	S >= Pthree,
	minimum(H, Pone, Ptwo, Min1),
	minimum(H, Ptwo, Pthree, Min2),
	heap_entry(H, Min1, K1, _),
	heap_entry(H, Min2, K2, _),
	K1 < K2,
	Min is Min1,
	!.

minimum(H, Pone, Ptwo, Pthree, Min) :-
	nonvar(H),
	nonvar(Pone),
	nonvar(Ptwo),
	nonvar(Pthree),
	heap(H, S),
	S >= Pone,
	S >= Ptwo,
	S >= Pthree,
	minimum(H, Pone, Ptwo, Min1),
	minimum(H, Ptwo, Pthree, Min2),
	heap_entry(H, Min1, K1, _),
	heap_entry(H, Min2, K2, _),
	K1 >= K2,
    Min is Min2,
	!.

heapify(H, _) :- 
	nonvar(H),
	empty(H),
	!.

heapify(H, _) :-
	nonvar(H),
	not_empty(H),
	heap(H, 1),
	!.

heapify(H, Node) :- 
	nonvar(H),
	not_empty(H),
	heap(H, S),
	S < Node,
	!.

heapify(H, Node) :- 
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	S >= Node,
	Left is 2 * Node,
	Left > S,
	Right is ((2 * Node) + 1),
	Right > S,
	!.

heapify(H, Node) :- 
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	S >= Node,
	Left is 2 * Node,
	S >= Left,
	Right is ((2 * Node) + 1),
	Right > S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Left, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Min),
	Node = Min,
	heapify(H, Left),
	!.

heapify(H, Node) :-
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	S >= Node,
	Left is 2 * Node,
	S >= Left,
	Right is ((2 * Node) + 1),
	Right > S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Min),
	Node \= Min,
	heap_entry(H, Min, Kmin, Vmin),
	heap_entry(H, Node, Knode, Vnode),
	scambio(H, Min, Kmin, Vmin, Node ,Knode, Vnode),
	heapify(H, Min),
	!.

heapify(H, Node) :- 
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	S >= Node,
	Left is 2 * Node,
	S >= Left,
	Right is ((2 * Node) + 1),
	Right > S,
	heap_entry(H, Left, _, _),
	heap_entry(H, Right, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Right, Min),
	Node = Min,
	!.

heapify(H, Node) :-
	nonvar(H),
	heap(H, S),
	S \= 0,
	S \= 1,
	S >= Node,
	Left is 2 * Node,
	S >= Left,
	Right is ((2 * Node) + 1),
	S >= Right,
	heap_entry(H, Left, _, _),
	heap_entry(H, Right, _, _),
	heap_entry(H, Node, _, _),
	minimum(H, Node, Left, Right, Min),
	Node \= Min,
	heap_entry(H, Min, Kmin, Vmin),
	heap_entry(H, Node, Knode, Vnode),
	scambio(H, Min, Kmin, Vmin, Node ,Knode, Vnode),
	heapify(H, Min),
	!.

propetyHeap(H, _) :- 
	nonvar(H),
	heap(H, _),
	empty(H),
	!.

propetyHeap(H, _) :-
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S = 1,
	!.

propetyHeap(H, Node) :-
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S \= 1,
	Node = 1,
	!.

propetyHeap(H, Node) :-
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S \= 1,
	Node \= 1,
	heap_entry(H, Node, _, _),
	Father is floor(Node / 2),
	heap_entry(H, Father, _, _),
	minimum(H, Node, Father, Min),
	Min = Father,
	!.

propetyHeap(H, Node) :-
	nonvar(H),
	heap(H, S),
	not_empty(H),
	S \= 1,
	Node \= 1,
	heap_entry(H, Node, Knode, Vnode),
	Father is floor(Node / 2),
	heap_entry(H, Father, Kfather, Vfather),
	minimum(H, Node, Father, Min),
	Min \= Father,
	scambio(H, Node, Knode, Vnode, Father, Kfather, Vfather),
	propetyHeap(H, Father),
	!.

% DIJKSTRA

max_int(Max) :-
	Max is 40000,
	!.

initialise_single_source(G, Source, Heap, Vs) :- 
	nonvar(G),
	nonvar(Source),
	nonvar(Heap),
	nonvar(Vs),
	graph(G),
	vertex(G, Source),
	heap(Heap, _),
	Vs = [],
	!.

initialise_single_source(G, Source, H, [V | Vs]) :-
	nonvar(G),
	nonvar(Source),
	nonvar(H),
	nonvar(V),
	nonvar(Vs),
	graph(G),
	vertex(G, Source),
	heap(H, _),
	[V | Vs] \= [],
	vertex(G, V),
	max_int(Max),
	insert(H, Max, V),
	assert(dist(G, V, Max)),
	assert(previous(G, V, null)),
	initialise_single_source(G, Source, H, Vs),
	!.

dijkstraneighbor(G, V, H, Ns) :-  
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	empty(H),
	!.

dijkstraneighbor(G, V, H, Ns) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	Ns = [],
	!.

dijkstraneighbor(G, V, H, [N | Ns]) :- 
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	arc(G, V, U, _) = N,
	dist(G, V, _),
	\+ heap_entry(H, _, _, U),
	dijkstraneighbor(G, V, H, Ns),
	!.

dijkstraneighbor(G, V, H, [N | Ns]) :- 
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	arc(G, V, U, Weight) = N,
	dist(G, V, D),
	heap_entry(H, _, OldKey, U),
	NewKey is (D + Weight),
	OldKey > NewKey,
	modify_key(H, NewKey, OldKey, U),
	change_dist(G, U, NewKey),
	change_previous(G, U, V),
	dijkstraneighbor(G, V, H, Ns),
	!.

dijkstraneighbor(G, V, H, [N | Ns]) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	arc(G, V, U, Weight) = N,
	dist(G, V, D),
	heap_entry(H, _, OldKey, U),
	NewKey is (D + Weight),
	NewKey >= OldKey,
	dijkstraneighbor(G, V, H, Ns),
	!.

dijkstra(G, V, H) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, _),
	empty(H),
	!.

dijkstra(G, V, H) :- 
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, S),
	not_empty(H),
	S = 1,
	neighbors(G, V, Ns),
	dijkstraneighbor(G, V, H, Ns),
	assert(visited(G, V)),
	dist(G, V, D),
	extract(H, D, V),
	!.

dijkstra(G, V, H) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, S),
	not_empty(H),
	S \= 1,
	neighbors(G, V, Ns),
	dijkstraneighbor(G, V, H, Ns),
	assert(visited(G, V)),
	dist(G, V, D),
	extract(H, D, V),
	head(H, _, U),
	dijkstra(G, U, H),
	!.