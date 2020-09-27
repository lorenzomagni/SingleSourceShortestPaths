Componenti del gruppo:

;;;; 845087 Manganaro Francesco
;;;; 816114 Magni Lorenzo
;;;; 829745 Colombo Filippo


Descrizione del programma svolto (sssp.lisp)

(is-graph graph-id)
	Ritorna il grafo stesso oppure NIL

(delete-graph graph-id)
	Elimina il grafo con id = graph-id

(new-graph graph-id)
	Permette di inizializzare un nuovo grado con id = graph-id

(new-vertex graph-id vertex-id)
	Permette di inizializzare un nuovo vertice nel grafo

(new-arc graph-id vertex1 vertex2 weight)
	Permette di inizializzare un nuovo arco
	[l'inserimento del peso dell'arco e' opzionale]

(graph-vertices graph-id)
	Crea una lista con tutti i vertici presenti in un grafo

(graph-arcs graph-id)
	Crea una lista con tutti gli archi all'interno di un grafo

(graph-vertex-neighbors graph-id vertex-id)
	Crea una lista con tutti i gli archi uscenti da un determinato vertice

(graph-print graph-id)
	Stampa le coppied chiavi/valore del grafo con id = graph-id


;;;; sssp

(sssp-dist graph-id vertex-id)
	Ritorna la distanza del vertice dal punto di origine

(sssp-visited graph-id vertex-id)
	Risulta true quando vertex-id risulta "visitato" durante o dopo lo svolgimento dell'algoritmo

(sssp-previous graph-id vertex-id)
	Ritorna il vertice che e' stato visita precedentemente a vertex-id
	Metodo che funziona solo dopo l'esecuzione dell'algoritmo

(sssp-change-dist graph-id vertex-id new-dist)
	Permette di modificare la distanza dal vertice all'origine

(sssp-change-previous graph-id vertex-id new-previous)
	Permette di modificare il vertice che è stato visitato precedentemente a vertex-id


(sssp graph-id vertex-id)
	Permette di applicare l'argoritmo di Dijkstra
	Richiama la funzione ausiliaria (sssp-aus graph-id) che permette di procedere con l'esecuzione
	senza inizializzare nuovamente l'heap (dato che viene inizializzato all'inizio del metodo sssp)
	
	Richiama poi le funzioni (vertex-up) e (vertex-up-aus) che servono per aggiornare i vertici durante
	l'esecuzione dell'algoritmo
	All'interno di vertex-up-aus viene richiamato (heap-modify-dist)
		Metodo che permette di scambiare il valore di due chiavi che sono prese in input
		Richiama poi la funzione (find-element)
			Viene utilizzata per trovare all'interno dell'heap l'elemento che deve essere modificato


(sssp-shortest-path graph-id source vertex)
	Richiamo la funzione (sssp-shortest-path graph-id source vertex ())
	Passando una lista vuota che poi verrà riempita dagli archi che compongono la strada minore
	dalla sorgente al vertice di arrivo

(sssp-shortest-path graph-id source vertex)
	Compone la lista di archi

(sssp-arcs-sssp graph-id vertex1 vertex2)
	Costruisce una lista con tutti gli archi che passano per due determinati vertici
	Utilizzata per scegliere l'arco di peso minore nel caso di scelte multiple


;;;; MinHeap

(new-heap heap-id capacity)
	Inizializza un nuovo heap con la capacita' fornita in input (opzionale)

(heap-id heap-id)
	Ritorna l'id dell'heap

(heap-size heap-id)
	Ritorna la dimensione dell'heap

(heap-actual-heap heap-id)
	Ritorna il valore dell'heap

(heap-capacity heap-id)
	Metodo ausiliario che ritorna la capacita' dell'heap

(heap-delete heap-id)
	Elimina l'heap indicizzato "heap-id"

(heap-empty heap-id)
	Torna true se l'heap e' vuoto

(heap-not-empty heap-id)
	Torna true se l'heap non e' vuoto

(heap-head heap-id)
	Ritorna la testa dell'heap

(heap-print heap-id)
	Stampa l'heap


(heap-insert heap-id key value)
	Inserisce l'elemento all'intero dell'heap
	Richiama la funzione (order-element) per ripristinare la Heap-Priority
		A sua volta order-element richiama (swap-element) e (swap-other) che si occupano
		di invertire gli elementi e ristabilire ordine nell'heap

(heap-extract heap-id)
	Estrae un elemento dall'heap
	Richiama la funzione (order-element-ex) per ripristinare la Heap-Priority
		A sua volta order-element richiama (swap-to-nill) e (swap-other)-ex che si occupano
		di invertire gli elementi e ristabilire ordine nell'heap

(heap-modify-key heap-id Nkey Okey value)
	Sostituisce la vecchia chiave con la nuova chiave
	Per ritrovare la posizione della chiave da sostituire viene utilizzata la funzione (find-position)
	Ristabilisce successivamente la Heap-Priority con le funzioni 
		(heap-shift)
		(heap-shift-right) --> shift destro dell'heap
		(heap-shift-left) --> shift sinistro dell'heap