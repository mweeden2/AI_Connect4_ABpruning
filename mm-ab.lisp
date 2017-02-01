;;;*******************************************************************************************
;;;*											     *
;;;*			Minimax and alpha-beta procedures (integrated)                       *	
;;;*											     *
;;;*******************************************************************************************
;;;
;;;
;;;*******************************************************************************************
;;;  Top level decision procedure to implement minimax procedure.  If the cutoff
;;; depth is reached, it evaluates the board with static_eval and the number of nodes
;;; evaluated in the current search process is updated.  If the cutoff
;;; depth is not reached, find out if the current board contains a winning or losing
;;; configuration.  If so, return the winning or losing value and the incremented
;;; boards-evaluated.  If not, evaluate the current board by calling either max-value or
;;; min-value depending on whether it is my turn to make a move. Returns a list of three numbers: 
;;; (i) the number of boards evaluated, 
;;; (ii) the slot corresponding to the successor with the best evaluation, and 
;;; (iii) that value.
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************

(defun minimax-decision (board level alpha-beta?
			       &optional (my-turn t) (alpha (list 0 nil *MINUS-INFINITY*))
			       (beta (list 0 nil *PLUS-INFINITY*)) (boards-evaluated 0))
  (if (<= level 0)
      (list (1+ boards-evaluated) nil (static_eval board))
    (cond ((terminalp board t)
	   ;; add the level so that closer wins are prefered; 5 is returned for slot 5
	   (list (1+ boards-evaluated) 5 (+ *WIN* level)))
	  ((terminalp board nil)
	   (list (1+ boards-evaluated) 5 (- *LOSE* level)))
	  (t
	   (if my-turn
	       (max-value board my-turn level alpha-beta? alpha beta boards-evaluated)
	     (min-value board my-turn level alpha-beta? alpha beta boards-evaluated))))))


;;;*******************************************************************************************
;;; This function first generates the children of the current board.  If this board has no
;;; children it is evaluated by static_eval and the corresponding value together with incremented
;;; boards-evaluated is returned.  If the input board has one or more children, they are 
;;; evaluated by calling the minimax procedure after decrementing the level and changing the 
;;; player.  This function then returns the maximum evaluation of the successor nodes. 
;;;   If alpha-beta pruning is to be performed, successors are evaluated until alpha
;;; becomes greater than or equal to beta.  If that happens, beta is returned, otherwise
;;; alpha is returned.  In both cases, the number of boards evaluated and the preferred move (slot)
;;; is returned.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;        boards-evaluated - number of boards evaluated so far
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************

(defun max-value (board player level alpha-beta? alpha beta boards-evaluated)
  (let ((color (get-color player)))
    ;; if the current board has no successor
    (if (eq (mapcar #'null (mapcar #'first (successors board color)))
            (make-list *DIMENSION* :initial-element t))
	(list (1+ boards-evaluated) -1 (static_eval board))
      (if alpha-beta?
	  (max-successors-ab board player (1- level) alpha-beta? alpha beta boards-evaluated)
	(max-successors board player (1- level) alpha-beta? alpha beta boards-evaluated)))))


;;;*******************************************************************************************
;;; This function returns the maximum evalutation of the successor nodes after calling 
;;; minimax-decision on each of the successors of board.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherise
;;;        boards-evaluated - number of boards evaluated so far
;;;        sccrs - list of successors of board that max-successors iterates through
;;;        count - argument used to keep track of chosen slot
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************

(defun max-successors (board player level alpha-beta? alpha beta boards-evaluated 
			     &optional (sccrs (successors board (get-color player))) (count 0))
  (if (null (second sccrs))		; if this is the last successor in the list
      (if (null (first (first sccrs)))	; if this is an empty successor
	  (if alpha-beta?
	      (list boards-evaluated count -1)
	    (list boards-evaluated count -1))
	(let ((mm (minimax-decision (first (first sccrs)) 
				    level alpha-beta? (not player) alpha beta boards-evaluated)))
	  (list (first mm) count (third mm))))
    (if (null (first (first sccrs)))	; if this is an empty successor
	(max-successors board player 
			level alpha-beta? alpha beta boards-evaluated (rest sccrs) (1+ count))
      (let ((first-eval (minimax-decision (first (first sccrs)) level 
					  alpha-beta? (not player) alpha beta boards-evaluated))
	    (rest-eval (max-successors board player level alpha-beta? alpha beta
				       boards-evaluated (rest sccrs) (1+ count))))
	;; if this successor has a better evaluation than the rest of the successors
	(if (>= (third first-eval) (third rest-eval))
	    (list (+ (first first-eval) (first rest-eval)) count (third first-eval))
	  (list (1+ (first rest-eval)) (second rest-eval) (third rest-eval)))))))

;;;*******************************************************************************************
;;; This function uses alpha-beta pruning and returns the maximum evalutation of the successor
;;; nodes after calling minimax-decision on each of the successors of board.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;        boards-evaluated - number of boards evaluated so far
;;;        sccrs - list of successors of board that max-successors-ab iterates through
;;;        count - argument used to keep track of chosen slot
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************  

(defun max-successors-ab (board player level alpha-beta? alpha beta boards-evaluated
                             &optional (sccrs (successors board (get-color player))) (count 0))
  (if (null (second sccrs))             ; if this is the last successor in the list
      (if (null (first (first sccrs)))  ; if this is an empty successor
          (list boards-evaluated count -1)
        (let ((mm (minimax-decision (first (first sccrs)) level alpha-beta? (not player) alpha beta boards-evaluated)))
          (list (first mm) count (third mm))))
    (if (null (first (first sccrs)))    ; if this is an empty successor
        (max-successors board player level alpha-beta? alpha beta boards-evaluated (rest sccrs) (1+ count))
      (let ((first-eval (minimax-decision (first (first sccrs)) level alpha-beta? (not player) alpha beta
                                          boards-evaluated)))
        (if (>= (third first-eval) (third alpha))
            (setf alpha first-eval))
        (if (>= (third alpha) (third beta))
            beta
          (let ((rest-eval (max-successors-ab board player level alpha-beta? alpha beta
                                          boards-evaluated (rest sccrs) (1+ count))))
            ;; if this successor has a better evaluation than the rest of the successors
            (if (>= (third first-eval) (third rest-eval))
                (list (+ (first first-eval) (first rest-eval)) count (third first-eval))
              (list (1+ (first rest-eval)) (second rest-eval) (third rest-eval)))))))))

;;;*******************************************************************************************
;;; This function first generates the children of the current board.  If this board has no
;;; children it is evaluated by static_eval and the corresponding value together with incremented
;;; boards-evaluated is returned.  If the input board has one or more children, they are 
;;; evaluated by calling the minimax procedure after decrementing the level and changing the 
;;; player.  This function then returns the minimum evaluation of the successor nodes. 
;;;   If alpha-beta pruning is to be performed, successors are evaluated until alpha
;;; becomes greater than or equal to beta.  If that happens, alpha is returned, otherwise
;;; beta is returned.  In both cases, the number of boards evaluated and the preferred move (slot)
;;; is returned.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************

(defun min-value (board player level alpha-beta? alpha beta boards-evaluated)
  (let ((color (get-color player)))
    ;; if there is no successor to the current board
    (if (eq (mapcar #'null (mapcar #'first (successors board color)))
	    (make-list *DIMENSION* :initial-element t))
        (list (1+ boards-evaluated) -1 (static_eval board))
      (if alpha-beta?
	  (min-successors-ab board player (1- level) alpha-beta? alpha beta boards-evaluated)
	(min-successors board player (1- level) alpha-beta? alpha beta boards-evaluated)))))
		
;;;*******************************************************************************************
;;; this function returns the minimum evalutation of the successor nodes after calling
;;; minimax-decision on each of the successors of board.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;        boards-evaluated - number of boards evaluated so far
;;;        sccrs - list of successors of board that min-successors iterates through
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************
(defun min-successors (board player level alpha-beta? alpha beta boards-evaluated
                             &optional (sccrs (successors board (get-color player))))
  (if (null (second  sccrs)) ; if there is only one successor in the list
      (if (null (first (first sccrs)))	; if this is an empty successor
          (list boards-evaluated nil 1)
        (minimax-decision (first (first sccrs)) level alpha-beta? (not player) alpha beta 
			  boards-evaluated))
    (if (null (first (first sccrs)))   ; if this is an empty successor
        (min-successors board player level alpha-beta? alpha beta boards-evaluated (rest sccrs))
      (let ((first-eval (minimax-decision (first (first sccrs)) level alpha-beta? (not player) 
						 alpha beta boards-evaluated))
            (rest-eval (min-successors board player level alpha-beta? alpha beta
                                       boards-evaluated (rest sccrs))))
        ;; if this successor has a better (more negative) evaluation than the rest of the successors
        (if (<= (third first-eval) (third rest-eval))
            (list (+ (first first-eval) (first rest-eval)) nil (third first-eval))
          (list (1+ (first rest-eval)) nil (third rest-eval)))))))

;;;*******************************************************************************************                             
;;; This function uses alpha-beta pruning and returns the minimum evalutation of the successor                             
;;; nodes after calling minimax-decision on each of the successors of board.                                               
;;; INPUT: board - board to be evaluated                                                                                   
;;;        player - t, if computer is to play, nil otherwise                                                               
;;;        level - current depth of search                                                                                 
;;;        alpha, beta - pruning parameters                                                                                
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise                                         
;;;        boards-evaluated - number of boards evaluated so far
;;;        sccrs - list of successors of board that min-successors-ab iterates through
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;******************************************************************************************* 

(defun min-successors-ab (board player level alpha-beta? alpha beta boards-evaluated
                             &optional (sccrs (successors board (get-color player))))
  (if (null (second  sccrs)) ; if there is only one successor in the list
      (if (null (first (first sccrs)))  ; if this is an empty successor
          (list boards-evaluated nil 1)
        (minimax-decision (first (first sccrs)) level alpha-beta? (not player) alpha beta
                          boards-evaluated))
    (if (null (first (first sccrs)))   ; if this is an empty successor
        (min-successors board player level alpha-beta? alpha beta boards-evaluated (rest sccrs))
      (let ((first-eval (minimax-decision (first (first sccrs)) level alpha-beta? (not player)
                                                 alpha beta boards-evaluated)))
        (if (<= (third first-eval) (third beta))
            (setf beta first-eval))
        (if (<= (third beta) (third alpha))
            beta
          (let ((rest-eval (min-successors-ab board player level alpha-beta? alpha beta
                                           boards-evaluated (rest sccrs))))
            ;; if this successor has a better (more negative) evaluation than the rest of the successors
            (if (<= (third first-eval) (third rest-eval))
                (list (+ (first first-eval) (first rest-eval)) nil (third first-eval))
              (list (1+ (first rest-eval)) nil (third rest-eval)))))))))

;;;*******************************************************************************************
;;; This function returns true if the current board is a winning board for the given player
;;; INPUT: board - board to be checked
;;;        player - t, if computer is to play, nil otherwise
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************

(defun terminalp (board player)
  (if (and (eq (num_occupied_positions (convert_to_array board) 4 player) 0)
	  (eq (num_occupied_positions (convert_to_array board) 5 player) 0)
	  (eq (num_occupied_positions (convert_to_array board) 6 player) 0))
      nil
    t))

;;;*******************************************************************************************
;;; This function is used for debugging; it prints a list of the evaluations of the board's 
;;; successors
;;; INPUT: board - board to be checked
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*******************************************************************************************

(defun stat (board player)
  (let ((s (successors board (get-color player))))
    (format t "~&eval 0: ~a~&eval 1: ~a~&eval 2: ~&eval 3: ~a~&eval 4: ~a~&eval 5: ~a~&"
	    (if (not (null (first (first s))))
		(static_eval (first (first s))))
	    (if (not (null (first (nth 1 s))))
		(static_eval (first (nth 1 s))))
	    (if (not (null (first (nth 2 s))))
                (static_eval (first (nth 2 s))))
	    (if (not (null (first (nth 3 s))))
                (static_eval (first (nth 3 s))))
	    (if (not (null (first (nth 4 s))))
                (static_eval (first (nth 4 s))))
	    (if (not (null (first (nth 5 s))))
                (static_eval (first (nth 5 s)))))))