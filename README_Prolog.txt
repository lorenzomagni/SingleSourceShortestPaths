Componenti del gruppo:

Colombo Filippo 829745
Magni Lorenzo 816114
Manganaro Francesco 845087

===

Implementazione dell'algoritmo di Dijkstra in Prolog

===

**** INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI DATI ****

Ogni grafico è definito da una serie di fatti salvati nel dataset:

graph(G) -> Il grafico denominato G

vertex(G, V) -> Il vertice V del grafo G

arc(G, V, U, Weight) -> Arco tra i vertici V e U, con peso non negativo

I predicati implementati dall'API sono i seguenti:

new_graph(G)  ->  Istanziare il grafico G nel database prolog.

delete(G)  ->  Rimuovere il grafo G e tutti i suoi vertici e archi.

add_vertex(G, V)  ->  Aggiungi il vertice V del grafo G al database.

vertices(G, Vs) - >  Questo predicato è vero quanto Vs è una lista contenente tutti i vertici di G .

list_vertices(G) ->  Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici del grafo G 

new_arc(G, V, U, Weight) -> Aggiunge un arco del grafo G alla base dati Prolog, con peso non negativo.

arcs(G, Es) -> Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.

neighbors(G, V, Ns) ->  Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi, arc(G, V, N, W), che portano ai vertici N immediatamente raggiungibili da V .

list_arcs(G) ->  Questo predicato stampa alla console dell’interprete Prolog una lista degli archi del grafo G.

list_grap(G)  ->  Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici e degli archi del grafo G.

===

**** MINHEAP IN PROLOG ****

I predicati implementati dall'API sono i seguenti:

new_heap(H)  ->  Questo predicato inserisce un nuovo heap nella base-dati Prolog.

delete_heap(H)  ->  Rimuove tutto lo heap (incluse tutte le “entries”) dalla base-dati Prolog.

heap_size(H, S) ->  Questo predicato è vero quanto S è la dimensione corrente dello heap. Una semplice implementazione potrebbe essere:

empty(H) -> Questo predicato è vero quando lo heap H non contiene elementi.

not_empty(H) ->  Questo predicato è vero quando lo heap H contiene almeno un elemento.

head(H, K, V) ->  Il predicato head/3 è vero quando l’elemento dello heap H con chiave minima K è V.

insert(H, K, V) -> Il predicato insert/3 è vero quando l’elemento V è inserito nello heap H con chiave K.

extract(H, K, V) ->  Il predicato extract/3 è vero quando la coppia K, V con K minima, è rimossa dallo heap H.

modify_key(H, NewKey, oldKey, V) ->  Il predicato modify_key/4 è vero quando la chiave OldKey (associata al valore V) è sostituita da NewKey. 

list_heap(H) ->  Questo predicato stampa alla console dell’interprete Prolog lo stato interno dello heap.

===

**** SSSP IN PROLOG ****

visited(G, V) ->  Questo predicato è vero quando V è un vertice di G e, durante e dopo l’esecuzione dell’algoritmo di Dijkstra, V risulta “visitato”.

dist(G, V, D) ->  Questo predicato è vero quando V è un vertice di G e, durante e dopo l’esecuzione dell’algoritmo di Dijkstra, la distanza minima del vertice V dalla “sorgente” è D.

previous(G, V, U) ->  Questo predicato è vero quando V ed U sono vertici di G e, durante e dopo l’esecuzione dell’algoritmo di Dijkstra, il vertice U è il vertice “precedente” a V nel cammino minimo dalla “sorgente” a V.

I predicati implementati dall'API sono i seguenti:

change_dist(G, V, NewDist) ->  Questo predicato ha sempre successo con due effetti collaterali: prima tutte le istanze di dist(G, V, _) sono ritirate dalla base-dati Prolog, e quindi dist(G, V, NewDist) è asserita.

change_previous(G, V, U) ->  Questo predicato ha successo con due effetti collaterali: prima tutte le istanze di previous(G, V, _) sono ritirate dalla base-dati Prolog, e quindi previous(G, V, U) è asserita.


sssp(G, Source) ->  Questo predicato ha successo con un effetto collaterale. Dopo la sua prova, la base-dati Prolog ha al suo interno i predicati dist(G, V, D) per ogni V appartenente a G; la base-dati Prolog contiene anche i predicati previous(G, V, U) e visited(V) per ogni V, ottenuti durante le iterazioni dell’algoritmo di Dijkstra. 

shortest_path(G, Source, V, Path) ->  Questo predicato è vero quando Path è una lista di archi.