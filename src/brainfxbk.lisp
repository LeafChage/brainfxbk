(in-package :cl-user)
(defpackage brainfxbk
  (:use :cl
        :cl-ppcre
        :split-sequence)
  (:export #:main
           #:main-for-code))
(in-package :brainfxbk)

(deftype token ()
  '(member :pinc :pdec :inc :dec :write :read :start :end))

(defvar *token* '((:pinc  . "\\>")
                  (:pdec  . "\\<")
                  (:inc   . "\\+")
                  (:dec   . "\\-")
                  (:write . "\\.")
                  (:read  . "\\,")
                  (:start . "\\[")
                  (:end   . "\\]")))

(defun main (argv)
  (run (parse
         (split-space (add-space (reaf-file (car argv))
                                 *token*)))))

(defun main-for-code (src)
  (run (parse
         (split-space (add-space src *token*)))))

(defun get-token (key)
  (cdr (assoc key *token*)))

(defmacro let1 ((key val) &body body)
  `(let ((,key ,val))
     ,@body))

(defun reaf-file (path)
  (with-open-file (s path :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun parse (arr)
  "シンボルに変更する"
  (let1 (tokens '())
        (dolist (token arr)
          (cond ((string= token (get-token :pinc))  (push :pinc  tokens))
                ((string= token (get-token :pdec))  (push :pdec  tokens))
                ((string= token (get-token :inc))   (push :inc   tokens))
                ((string= token (get-token :dec))   (push :dec   tokens))
                ((string= token (get-token :write)) (push :write tokens))
                ((string= token (get-token :read))  (push :read  tokens))
                ((string= token (get-token :start)) (push :start tokens))
                ((string= token (get-token :end))   (push :end   tokens))))
        (reverse tokens)))

(defun add-space (src tokens)
  "予約後の前後に半角スペースを入れる"
  (if (consp tokens)
    (let1 (s (car tokens))
          (add-space (ppcre:regex-replace-all
                       (cdr s)
                       src
                       (concatenate 'string " " (cdr s) " "))
                     (cdr tokens)))
    src))

(defun split-space (src)
  "半角スペースで分割する"
  (multiple-value-bind (list )
    (split-sequence:split-sequence #\Space src)
    list))


(defun run (tokens)
  "実行する"
  (let ((memory (make-array 100000000 :initial-element 0 :fill-pointer 0 :adjustable t))
        (pointer 0)
        (jump-point 0))
    (dotimes (i (list-length tokens))
      (case (nth i tokens)
        (:pinc (when (> (length memory) (incf pointer))
                 (vector-push 0 memory)))
        (:pdec  (decf pointer))
        (:inc   (incf (aref memory pointer)))
        (:dec   (decf (aref memory pointer)))
        (:write (format t "~A" (code-char (aref memory pointer))))
        (:read  (setf (aref memory pointer)
                      (char-code (read-char))))
        (:start (if (= (aref memory pointer) 0)
                  (loop while (not (eq (nth i tokens) :end))
                        do
                        (incf i))
                  (setf jump-point i)))
        (:end (setf i (1- jump-point)))))))

