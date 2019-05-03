;;; very crude script to partially pre-process the tests in the book to a testing macro

(defmacro comment (&rest args))

(comment "esempio di testo trasformabile con il comando sotto"
 Scenario ​: Reflecting a vector approaching at 45° ​ ​ Given ​ v ← vector(1, -1, 0) ​ ​ And ​ n ← vector(0, 1, 0) ​ ​ When ​ r ← reflect(v, n) ​ ​ Then ​ r = vector(1, 1, 0))

(defun replace-all (from to)
  (goto-char (point-min))      
  (while (re-search-forward from nil t)
    (replace-match to)))

(defun replace-newlines ()
  (replace-all " ​ ​ " (char-to-string ?\n)))

(defun replace-spaces ()
  (replace-all " ​" ""))

(defun wrap-all-with-title ()
  (goto-char (point-min))
  (if (re-search-forward "Scenario: ")
      (replace-match "")
    (error "'Scenario: ' not found where expected!"))
  (let ((start -1)
        (end (point-max)))    
    (insert "(testing \"")
    (goto-char (line-end-position))
    (insert "\"")
    (forward-char 1)
    (setq start (point))
    (setq end (point-max))
    (goto-char end)
    (insert ")")
    (narrow-to-region start end)))

(defun convert-to-test ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning)
                        (region-end))
      ;;; Replace the special characters with plain ASCII
      (replace-newlines)
      (replace-spaces)
      ;;; Insert deftesting etc, narrow to the inner part of the test
      (wrap-all-with-title)
      ;;; underline the separate parts of the test

      (goto-char (point-min))
      (when (re-search-forward "When")
        (backward-word 1)
        (insert (char-to-string ?\n)))
      (when (re-search-forward "Then")
        (backward-word 1)
        (insert (char-to-string ?\n))))))

