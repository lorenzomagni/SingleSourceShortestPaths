;;;; 845087 Manganaro Francesco
;;;; 816114 Magni Lorenzo
;;;; 829745 Colombo Filippo

;;;; sssp.lisp

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))

(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *dist* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defparameter *heaps* (make-hash-table :test #'equal))


(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  nil)



(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf 
       (gethash graph-id *graphs*) 
       graph-id)))

(defun new-vertex (graph-id vertex-id) 
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
        (list 'vertex graph-id vertex-id)))

(defun new-arc (graph-id vertex-id1 vertex-id2 &optional (weight 1))
  (setf (gethash 
         (list 'arc  graph-id vertex-id1 vertex-id2 weight) 
         *arcs*)
        (list 'arc graph-id vertex-id1 vertex-id2 weight)))



(defun graph-vertices (graph-id)
  (let ((chiave ()) (valore ()))
    (maphash (lambda (key value)
	       (cond
		((equal (second key) graph-id)
		 (push key chiave)
	       	 (push value valore))))
	     *vertices*)
    chiave))

(defun graph-arcs (graph-id)
  (let ((chiave ()) (valore ()))
    (maphash (lambda (key value)
	       (cond
		((equal (second key) graph-id)
		 (push key chiave)
		 (push value valore))))
	     *arcs*)
    chiave))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((chiave ()) (valore ()))
    (maphash (lambda (key value)
	       (cond
		((and
		 (equal (second key) graph-id)
		 (equal (third key) vertex-id))
		 (push key chiave)
		 (push value valore))))		 
	     *arcs*)
    chiave))



(defun graph-print (graph-id)
  (maphash
   (lambda (keys values)
     (cond
      ((equal (second keys) graph-id)
       (format t "Key ~S  Value ~S~%" keys values)))) 
   *vertices*)
  (maphash
   (lambda (keys values)
     (cond
      ((equal (second keys) graph-id)
       (format t "Key ~S  Value ~S~%" keys values)))) 
   *arcs*))




;;; SSSP

(defun sssp-dist (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *dist*))

(defun sssp-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

(defun sssp-visited (graph-id vertex-id)
  (setf 
   (gethash (list graph-id vertex-id) *visited*) 
   T))


(defun sssp-change-dist (graph-id vertex-id new-dist)
  (setf 
   (gethash (list graph-id vertex-id) *dist*) 
   new-dist)
  nil)

(defun sssp-change-previous (graph-id vertex-id new-previous)
  (setf 
   (gethash (list graph-id vertex-id) *previous*) 
   new-previous)
  nil)



(defun sssp (graph-id source-vertex)
  (progn
    (sssp-change-dist graph-id source-vertex 0)

    (new-heap 
     graph-id 
     (length (graph-vertices graph-id)))

    (let ((array 
           (graph-vertex-neighbors graph-id source-vertex)))
      (let ((dim (length array)))
        (vertex-up graph-id source-vertex array (- dim 1))))

    (sssp-visited graph-id source-vertex)

    (sssp-aus graph-id)
    nil))

(defun sssp-aus (graph-id)
  (cond((heap-not-empty graph-id)
	(let ((vertex
               (second(heap-extract graph-id))))

          (let ((array 
                 (graph-vertex-neighbors graph-id vertex)))
            (let ((dim (length array)))
              (vertex-up graph-id vertex array (- dim 1))))
          
	  (sssp-visited graph-id vertex)
	  (sssp-aus graph-id)))))

(defun vertex-up (graph-id vertex array dim)
  (cond((>= dim 0)

	(vertex-up-aus graph-id vertex array dim)

	(vertex-up graph-id vertex array (- dim 1)))))

(defun vertex-up-aus (graph-id vertex array point)
  (let ((vert 
         (fourth(nth point array)))

	(dim 
         (fifth(nth point array))))

    (let ((old-dist 
           (sssp-dist graph-id vert))

	  (new-dist 
           (+ (sssp-dist graph-id vertex) dim)))

      (cond

       ((equal old-dist nil)
        (progn
          (sssp-change-dist graph-id vert new-dist)
          (sssp-change-previous graph-id vert vertex)
          (heap-insert graph-id new-dist vert)))

       ((< new-dist old-dist)
        (progn
          (sssp-change-dist graph-id vert new-dist)
          (sssp-change-previous graph-id vert vertex)
          (heap-modify-dist graph-id new-dist old-dist)))))))

(defun heap-modify-dist (heap-id new-key old-key)
  (let ((list (heap-actual-heap heap-id))
	(element (find-element
                  (heap-actual-heap heap-id)
                  (- (heap-size heap-id) 1)
                  old-key)))
    (cond
     ((equal element nil) nil)

     (t
      (let ((value 
             (second (aref list element))))
        
	(setf 
         (aref list element) 
         (list new-key value))

	(heap-shift list (- (heap-size heap-id) 1) element))))))

(defun find-element (array size key-f)
  (cond
   ((< size 0) nil)

   (t
    (let ((key (first (aref array size))))
      (cond
       ((= key key-f) size)

       (t (find-element array (- size 1) key-f)))))))



; Inizializzazione con lista vuota
(defun sssp-shortest-path (graph-id source vertex)
  (sssp-shortest-path-aus graph-id source vertex ()))

; Creazione lista per risultato
(defun sssp-shortest-path-aus (graph-id source vertex elements)
  (let ((vertex-p 
         (sssp-previous graph-id vertex)))

    (cond((equal vertex source) elements)

	 (t
	  (push 
           (first (graph-arcs-sssp graph-id vertex-p vertex)) 
           elements)

	  (sssp-shortest-path-aus 
           graph-id 
           source 
           vertex-p elements)))))

; Trova tutti gli archi tra due vertici
(defun graph-arcs-sssp (graph-id vertex1 vertex2)
  (let ((chiavi ()) (valori ()))
    (maphash (lambda (key value)
	       (cond
		((and
		  (equal (second key) graph-id)
		  (equal (third key) vertex1)
		  (equal (fourth key) vertex2))
		 (push key chiavi)
		 (push value valori))))
	     *arcs*)
    chiavi))




;;; MinHeap

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))

    (second temp)))

(defun heap-size (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))

    (third temp)))

(defun heap-actual-heap (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))

    (fourth temp)))

(defun heap-capacity (heap-id)
  (let ((temp 
         (gethash heap-id *heaps*)))

    (length(fourth temp))))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))


(defun heap-empty (heap-id)
  (let ((temp
         (gethash heap-id *heaps*)))

    (cond 
     ((= (third temp) 0) 
      t))))

(defun heap-not-empty (heap-id)
  (let ((temp
         (gethash heap-id *heaps*)))

    (cond ((/= (third temp) 0) 
           t))))


(defun heap-head (heap-id)
  (aref (heap-actual-heap heap-id) 0))

(defun heap-print (heap-id)
  (print (gethash heap-id *heaps*)) t)



; Inserimento elemento
(defun heap-insert (heap-id key value)
  (let ((cap (heap-capacity heap-id))
	(size (heap-size heap-id))
        (list (gethash heap-id *heaps*)))
    (cond
     ((< size cap)
      (progn 
        (setf 
         (aref (fourth list) size) 
         (list key value))

        (setf 
         (third list) 
         (+ (third list) 1))

        (order-element heap-id)))

     (T (error "Full")))))

; Ordinamento elementi post inserimento (Ricostruzione Heap-Priority)
(defun order-element (heap-id)
  (let ((size 
         (heap-size heap-id)))

    (cond
     ((= size 1) t)
     ((= size 2) (swap-element heap-id 1 0))
     ((> size 2) (swap-other heap-id size)))))

; Inversione di due elementi
(defun swap-element (heap-id pos1 pos2)
  (let ((list 
         (heap-actual-heap heap-id)))

    (let ((first-el (first (aref list pos2)))
	  (second-el (first (aref list pos1))))

      (cond
       ((< second-el first-el)
	(let ((temp (aref list pos2)))
	  (setf 
           (aref list pos2) 
           (aref list pos1))

	  (setf 
           (aref list pos1) 
           temp)))))))

; Inversione di elementi multipli
(defun swap-other (heap-id size)
  (cond
   ((= size 1) t)

   (t
    (swap-element heap-id (- size 1) (- size 2))
    (swap-other heap-id (- size 1)))))



; Estrazione di un elemento
(defun heap-extract (heap-id)
  (let ((list 
         (heap-actual-heap heap-id))

        (l-for-decrease 
         (gethash heap-id *heaps*)))

    (let ((kval 
           (aref list 0)))

      (progn
        (setf 
         (aref list 0) 
         nil)

        (setf 
         (third l-for-decrease) 
         (- (third l-for-decrease) 1))

        (order-element-ex heap-id)

        kval))))

; Ordinamento elementi post estrazione (Ricostruzione Heap-Priority)
(defun order-element-ex (heap-id)
  (let ((size 
         (heap-size heap-id)))

    (cond
     ((= size 0) t)
     ((= size 1) (swap-to-nill heap-id 1 0))
     ((> size 1) (swap-other-ex heap-id 0 size)))))

; Inversione di due elementi
(defun swap-to-nill (heap-id pos1 pos2)
  (let ((list 
         (heap-actual-heap heap-id)))

    (setf 
     (aref list pos2) 
     (aref list pos1))

    (setf 
     (aref list pos1) 
     nil)))

; Inversione di elementi multipli
(defun swap-other-ex (heap-id initial size)
  (cond
   ((= initial size) t)

   (t
    (swap-to-nill heap-id (+ initial 1) initial)
    (swap-other-ex heap-id (+ initial 1) size))))



; Modifica di un elemento 
(defun heap-modify-key (heap-id new-key old-key value)
  (let ((list (heap-actual-heap heap-id))
	(position (find-position
                  (heap-actual-heap heap-id)
                  (- (heap-size heap-id) 1)
                  old-key
                  value)))
    (cond
     ((equal position nil) nil)

     (t
      (setf 
       (aref list position) 
       (list new-key value))

      (heap-shift list (- (heap-size heap-id) 1) position)))))

; Ritrovo posizione dell'elemento da modificare
(defun find-position (list size key value)
  (cond
   ((< size 0) nil)

   (t
    (let ((l-key (first (aref list size)))
          (l-value (second (aref list size))))

      (cond
       ((and (= l-key key)(= l-value value)) size)

       (t 
        (find-position list (- size 1) key value)))))))

; Ricostruzione Heap-Priority
(defun heap-shift (list size point)
  (cond
   ((= point 0) (heap-shift-right list size point))

   ((= point size) (heap-shift-left list size point))

   (t
    (heap-shift-right list size point)
    (heap-shift-left list size point))))

; Shift destro heap
(defun heap-shift-right (list size point)
  (cond
   ((= size point) t)

   ((> (first(aref list point)) (first(aref list (+ point 1))))
    (let ((temp 
           (aref list point)))

      (setf 
       (aref list point) 
       (aref list (+ point 1)))

      (setf 
       (aref list (+ point 1)) 
       temp)

      (heap-shift-right list size (+ point 1))))))

; Shift sinistro heap
(defun heap-shift-left (list size point)
  (cond
   ((= point 0) t)

   ((< (first(aref list point)) (first(aref list (- point 1))))
    (let ((temp 
           (aref list point)))

      (setf 
       (aref list point) 
       (aref list (- point 1)))

      (setf 
       (aref list (- point 1)) 
       temp)

      (heap-shift-left list size (- point 1))))))



