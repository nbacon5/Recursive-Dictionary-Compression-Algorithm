(defparameter *substrings* (make-hash-table :test #'equal))
;substring:(count index value)
;ex: abcde abcde abcde abcde abcde
;"abcde":(5 0 15)

(defun file-to-string (filename)
  (with-open-file (file filename)
    (let ((text (make-string (file-length file))))
      (read-sequence text file)
      text)))

(defun set-value (substring value)
    (let* ((counter (first value))
          (len (length substring)))
      (setf (gethash substring *substrings*) 
            (list (first value) (second value) 
                  (- (- (* len counter) counter) len)))))

(defun get-var ()
  (let ((letter 64) (num 0))
    (lambda ()
      (incf letter)
      (if (> letter 90)
        (progn
          (setf letter 65)
          (incf num)))
      (concatenate 'string
                   "$" (string (code-char letter)) 
                   (write-to-string num) "$"))))

(defun compress (text)
  (print text)
  (dotimes (i (length text)) ; compute substring counts
    (dotimes (j (- (length text) (1+ i)))
      (let* ((substring (subseq text i (+ j (1+ i))))
             (value (gethash substring *substrings*)))
        (if (null value)
          (setf (gethash substring *substrings*) (list 1 (+ j i) 0))
          (if (> i (second value)) ; current instance is not in previous instance
            (setf (gethash substring *substrings*) 
                  (list (1+ (first value)) (+ j i) 0)))))))
  (maphash (lambda (k v) (set-value k v)) *substrings*) ; set substring value
  (let ((substrings) (generator (get-var))) ; substrings sorted by value
    (maphash (lambda (k v) (if (> (third v) 0) (push (cons k (third v)) substrings))) 
             *substrings*)
    (setf substrings (mapcar (lambda (s) (first s)) 
                                    (sort substrings 
                                          (lambda (a b) (> (cdr a) (cdr b))))))
        (let ((substring (first substrings))
              (index 0) 
              (result nil) 
              (var (funcall generator)))
          (loop
            (setf result (search substring text ; get each instance in text
                                :start2 (+ index (if (= 0 index) 0 (length var))) 
                                :test #'equal))
            (if (null result) 
              (return)
              (setf index result))
            (setf text (concatenate 'string ; replace instance with var
                                    (subseq text 0 index) var 
                                    (subseq text (+ index (length substring)) 
                                            (length text)))))))
  text)
            

(defun start (filename)
  (compress (file-to-string filename)))
  ;(maphash (lambda (k v) (format t "~A: ~A~% " k v)) *substrings*))
