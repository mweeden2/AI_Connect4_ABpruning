;;;********************************************************************************************
;;;*											      *
;;;*			Board Evaluation Routines					      *
;;;*											      *
;;;********************************************************************************************
;;;
;;; WINNING is a very high value returned when a winning configuration is detected.
;;; LOSING is a very low value returned when a losing configuration is detected.
;;;
(defvar *WINNING* (* 10 *WIN*))
(defvar *LOSING*  (* 10 *LOSE*))

;;*********************************************************************************************
;; This is the board-evaluator function called from minimax to evaluate a board. It returns an
;; integral value that reflect the promise of the input board.
;; INPUT: Current board
;;*********************************************************************************************
(defun static_eval (board)
  (rank_board (convert_to_array board)))

;;**********************************************************************************************
;; The following function convert an input board to array representation. It
;; is this array representation that is passed by the function static_eval() to 
;; the function rank_board() 
;; INPUT: current board.
;;*********************************************************************************************
(defun convert_to_array (board)
  (let ((index 0)
	(squares (make-array (* *Dimension* *Dimension*))))
    (dotimes (i *Dimension* squares)
	     (dotimes (j *Dimension*)
		      (setf (aref squares index)
			    (aref board i j))
		      (incf index)))))

;;**********************************************************************************************
;; This function, on being given the array representation of a board returns an integer that is
;; the rank of the board from the view-point of the maximising player. Note that the variable
;; win_positions is a list of all possible winning configurations.
;; INPUT: array representation of current board.
;;
;; This function was written by Matt Weeden 10/20/15
;;**********************************************************************************************
(defun rank_board (board_array)
  (cond ((or (> (num_occupied_positions board_array 4 t) 0)
	    (> (num_occupied_positions board_array 4 t) 0)
	    (> (num_occupied_positions board_array 4 t) 0))
	 *WIN*)
	((or (> (num_occupied_positions board_array 4 nil) 0)
	 (> (num_occupied_positions board_array 4 nil) 0)
	 (> (num_occupied_positions board_array 4 nil) 0))
	 *LOSE*)
	(t
	 (- (+ 1 (* (num_occupied_positions board_array 1 t) 2)
	       (* (num_occupied_positions board_array 2 t) 7)
	       (* (num_occupied_positions board_array 3 t) 16))
	    (+ (* (num_occupied_positions board_array 1 nil) 2)
               (* (num_occupied_positions board_array 2 nil) 7)
               (* (num_occupied_positions board_array 3 nil) 16))))))

;;;*********************************************************************************************
;;; This function returns the number of win positions that contain the given number of tokens
;;; INPUT: board_array - the board being evaluated
;;;        num-tokens - the number of tokens the function is checking for for  each win position
;;;        player - t means computer's turn, nil means opponent's turn
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*********************************************************************************************
(defun num_occupied_positions (board_array num-tokens player 
				     &optional (positions-count (length *win_positions*)))
  (if (eq positions-count 0)
      (if (position_occupiedp board_array num-tokens player (first *win_positions*))
	  1
	0)
    (if (position_occupiedp board_array num-tokens player (nth (1- positions-count) *win_positions*))
	(+ 1 (num_occupied_positions board_array num-tokens player (1- positions-count)))
      (num_occupied_positions board_array num-tokens player (1- positions-count)))))

;;;*********************************************************************************************
;;; This function returns a boolean indicating whether the given position is occupied by the 
;;; given number of player tokens and no oponent tokens in the given board
;;; INPUT: board_array - the board being evaluated
;;;        num-tokens - the number of tokens the function is checking for for  each win position
;;;        player - t means computer's turn, nil means opponent's turn
;;;        position - the current (win) position being evaluated
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*********************************************************************************************
(defun position_occupiedp (board_array num-tokens player position)
  (if (and (eq (num_occupying_tokens board_array player position) num-tokens)
	   (eq (num_occupying_tokens board_array (not player) position) 0))
      t
    nil))

;;;*********************************************************************************************
;;; This function returns the number of occupying tokens in the board for the given player
;;; in the space of the given position
;;; INPUT: board_array - the board being evaluated
;;;        player - t means computer's turn, nil means opponent's turn
;;;        position - the current (win) position being evaluated
;;;
;;; This function was written by Matt Weeden 10/20/15
;;;*********************************************************************************************
(defun num_occupying_tokens (board_array player position)
  (if (eq (length position) 1)
      (if (eq (aref board_array (first position))
	      (get-color player))
	  1
	0)
    (if (eq (aref board_array (first position))
	    (get-color player))
	(+ 1 (num_occupying_tokens board_array player (rest position)))
      (num_occupying_tokens board_array player (rest position)))))