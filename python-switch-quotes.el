;;; python-switch-quotes.el --- cycle between apostrophes and quotes in python strings
;;; Copyright (C) 2016 Vladimir Lagunov

;;; Author: Vladimir Lagunov <lagunov.vladimir@gmail.com>
;;; Maintainer: Vladimir Lagunov <lagunov.vladimir@gmail.com>
;;; URL: https://github.com/werehuman/python-switch-quotes
;;; Created: 2016-12-18
;;; Version: 0.1
;;; Keywords: python
;;; Package-Requires:

;;; Commentary:
;;;   Converts strings like 'this' to strings like "this".
;;;   Supports raw strings, docstrings and strings with escaped quotes.
;;;
;;;   Assigns key C-c ' to convert string at point.

;;; Code:
(require 'python)

(defun python-switch-quotes--simple (string-start string-end old-quote new-quote)
  "Private: \"hello world\" => 'hello world'."
  (goto-char (1- string-end))
  (save-excursion
    (delete-char 1)
    (insert new-quote))
  (while (re-search-backward "[\"']" string-start t)
    (let ((return-back (point)) (found-char (char-after (point))))
      (delete-char 1)
      (cond ((eq found-char new-quote)
             ;; "hello >>'<<world" => 'hello \'world'
             (insert ?\\ new-quote))
            ((eq (char-before return-back) ?\\)
             ;; "hello >>\"<<world" => 'hello "world'
             (delete-char -1)
             (insert old-quote)
             (setq return-back (1- return-back)))
            (t
             ;; >>"<<hello world" => 'hello world
             (insert new-quote)))
      (goto-char return-back))))


(defun python-switch-quotes--raw-simple (string-start string-end new-quote)
  "Private: r\"hello world\" => r'hello world'"
  (goto-char (1- string-end))
  (delete-char 1)
  (insert new-quote)
  (goto-char string-start)
  (delete-char 1)
  (insert new-quote))


(defun python-switch-quotes--docstring (string-start string-end old-quote new-quote)
  "Private: \"\"\"hello world\"\"\" => '''hello world'''."
  (goto-char string-start)
  (delete-char 3)
  (insert new-quote new-quote new-quote)
  (goto-char string-end)
  (delete-char -3)
  (cond
   ((eq (char-before (point)) new-quote)
    ;;; """hello world'""" => '''hello world\''''
    (delete-char -1)
    (insert ?\\ new-quote new-quote new-quote new-quote)
    (goto-char (- (point) 2)))
   ((and (eq (char-before (point)) old-quote)
         (eq (char-before (1- (point))) ?\\))
    ;;; """hello world\"""" => '''hello world"'''
    (delete-char -2)
    (insert old-quote new-quote new-quote new-quote)
    (goto-char (1- (point))))
   (t
    ;;; """hello world""" => '''hello world'''
    (insert new-quote new-quote new-quote)))
  (goto-char (- (point) 3))
  (let ((re (regexp-opt '("'''" "\"\"\"")))
        (bound (+ 3 string-start)))
    (while (re-search-backward re bound t)
      (when (and (eq new-quote (char-after (point)))
                 (not (eq ?\\ (char-before (point)))))
        (save-excursion (insert ?\\))))))


(defun python-switch-quotes--raw-docstring (string-start string-end new-quote)
  "Private: r\"hello world\" => r'hello world'."
  (goto-char (- string-end 3))
  (delete-char 3)
  (insert new-quote new-quote new-quote)
  (goto-char string-start)
  (delete-char 3)
  (insert new-quote new-quote new-quote))


;;;###autoload
(defun python-switch-quotes (&optional pos)
  "Convert apostrophe quoted string to quoted and vice versa.
POS - point inside of string, using current position if omitted."
  (interactive)
  (save-excursion
    (goto-char (or pos (point)))
    (let ((string-start (python-syntax-context 'string)))
      (if (not string-start) (error "Not a python string")
        (let* ((string-end (scan-sexps string-start 1))
               (old-quote (char-after string-start))
               (new-quote (if (equal old-quote ?\") ?' ?\"))
               (is-raw (memq (char-before string-start) '(?r ?R)))
               (is-docstring (equal (buffer-substring string-start (+ 3 string-start))
                                    (string old-quote old-quote old-quote))))
          (cond
            ((and is-raw is-docstring)
             (python-switch-quotes--raw-docstring string-start string-end new-quote))
            (is-docstring
             (python-switch-quotes--docstring string-start string-end old-quote new-quote))
            (is-raw
             (python-switch-quotes--raw-simple string-start string-end new-quote))
            (t
             (python-switch-quotes--simple string-start string-end old-quote new-quote))))))))

(define-key python-mode-map (kbd "C-c '") 'python-switch-quotes)

(provide 'python-switch-quotes)
;;; python-switch-quotes.el ends here
