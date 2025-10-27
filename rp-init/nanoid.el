
;; From: https://github.com/matoous/go-nanoid
;; https://github.com/ai/nanoid

(defvar default-alphabet "_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar default-size 21)

(defun random-char (alphabet)
  (let* ((index (random (length alphabet)))
         (char (aref alphabet index)))
    char))

(defun generate (alphabet len)
  (let* ((result-id (make-string len ?x)))
    (dotimes (i len)
      (aset result-id i (random-char alphabet)))
    result-id))
  
(defun new (&optional len)
  (generate default-alphabet (or len default-size)))
