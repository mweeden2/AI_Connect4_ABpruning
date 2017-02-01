;;;*******************************************************************************************
;;;*											     *
;;;*			Generate all possible Winning Configurations                         *	
;;;*											     *
;;;*******************************************************************************************

;;;*******************************************************************************************
;;; This function accepts a starting number,an increment and a count as arguments and returns
;;; the enumerated list. An example is : (enumerate 1 2 5) ==> (1 3 5 7 9).
;;;*******************************************************************************************
(defun enumerate (start increment count)
  (do ((n start (+ n  increment))
       (l nil (cons n l))
       (i count (1- i)))
      ((zerop i) (nreverse l))))

;;;*******************************************************************************************
;;; This function returns a list of the first elements of all horizontal winning sequences in
;;; a board.
;;; eg. If the board has dimension 6, this function returns all the array indices of the
;;; first 3 columns.
;;;*******************************************************************************************
(defun across (x)
  (if (< (- *Dimension* x) 4)
      nil
    (append (enumerate x *Dimension* *Dimension*) (across (1+ x)))))

;;;*******************************************************************************************
;;; This function returns  a list of the first elements of all vertical winning sequences
;;; in a board.
;;;*******************************************************************************************
(defun down ()
    (enumerate 0 1 (* (- *Dimension* 3) *Dimension*)))

;;;*******************************************************************************************
;;; The following two  functions return a list each of the first elements  of all diagonal
;;;  winning sequences. 
;;;*******************************************************************************************
(defun diagright (x y)
  (if (= x (- *Dimension* 3)) 
      nil
    (append (enumerate y 1 (- *Dimension* 3))
	    (diagright (1+ x) (+ *Dimension* y)))))

(defun diagleft (x y)
  (if (= x (- *Dimension* 3)) 
      nil
    (append (enumerate y -1 (- *Dimension* 3))
	    (diagleft (1+ x) (+ *Dimension* y)))))

;;;********************************************************************************************
;;; This function takes as input, the board-width and returns a list of all possible winning
;;; sequences for a board of that size.
;;;*******************************************************************************************
(defun possible_wins ()
    (append
	(mapcar #'(lambda (n)
		  (enumerate n 1 4))
	        (across 0))
	(mapcar #'(lambda (n)
		  (enumerate n *Dimension* 4))
	        (down))
	(mapcar #'(lambda (n)
		  (enumerate n (1+ *Dimension*) 4))
		(diagright 0 0))
	(mapcar #'(lambda (n)
		  (enumerate n (1- *Dimension*) 4))
		(diagleft 0 (1- *Dimension*)))))

;;;********************************************************************************************
;;; This variable is initialized to the list of all possible winning sequences. It is used by
;;; the function rank_board().
;;;*******************************************************************************************

(defvar *win_positions* (possible_wins))
