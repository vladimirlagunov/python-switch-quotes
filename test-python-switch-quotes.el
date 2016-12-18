;;; This file is not part of GNU Emacs.

;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

;;; How to run tests (with emacs 24 and greater):
;;; emacs -Q -batch -l ert -l python-switch-quotes.el -l test-python-switch-quotes.el -f ert-run-tests-batch-and-exit

(require 'python-switch-quotes)

(defun switch-it (py-string)
  (with-temp-buffer
    (setq python-indent-guess-indent-offset nil)
    (python-mode)
    (insert py-string)
    (goto-char (/ (buffer-end 1) 2))
    (python-switch-quotes)
    (buffer-string)))

(defun python-approves (py-string py-string1)
  (let* ((source (concat
                 "a = " py-string "\n"
                 "b = " py-string1 "\n"
                 "print('01'[a == b])\n"))
         (result (with-temp-buffer
                   (call-process
                    "python"
                    nil  ; input file
                    t  ; output into current buffer
                    nil  ; no redisplay
                    "-c" source)
                   (buffer-string))))
    (eq (string-to-char result) ?1)))


(defmacro test-one-fixture (fixture)
  `(let ((py-string) (py-string1) (py-string2))
     (setq py-string ,fixture)
     (setq py-string1 (switch-it py-string))
     (should (python-approves py-string py-string1))
     (setq py-string2 (switch-it py-string1))
     (should (python-approves py-string1 py-string2))))


(ert-deftest test-in-python ()
  ;;; simple strings
  (test-one-fixture "'hello world'")
  (test-one-fixture "\"hello world\"")  

  (test-one-fixture "'hello \\u0022 \\u0027 world\\n'")

  (test-one-fixture "'\\'hello world'")
  (test-one-fixture "'hello world\\''")
  (test-one-fixture "'hello \" world'")
  (test-one-fixture "'he\\'llo \\'\\'\\' world'")
  (test-one-fixture "\"he'llo ''' world\"")

  ;;; docstrings
  (test-one-fixture "'''hello world'''")
  (test-one-fixture "\"\"\"hello world\"\"\"")

  (test-one-fixture "'''hello 'quoted' world'''")
  (test-one-fixture "'''hello \"quoted\" world'''")

  (test-one-fixture "'''hello world\"'''")
  (test-one-fixture "\"\"\"hello world'\"\"\"")

  (test-one-fixture "'''\\'''hello \\''' world\\'\\'\\''''")
  (test-one-fixture "\"\"\"\\\"\"\"hello \\\"\"\" world\\\"\\\"\\\"\"\"\"")

  ;;; simple raw strings
  (test-one-fixture "r'hello world'")
  (test-one-fixture "R\"hello world\"")

  (test-one-fixture "r'hello \\' world'")
  (test-one-fixture "r\"hello \\' world\"")
  (test-one-fixture "r'hello \\\" world'")
  (test-one-fixture "r\"hello \\\" world\"")

  (test-one-fixture "r'\\'hello'")
  (test-one-fixture "r'hello\\''")
  (test-one-fixture "r'\\\"hello'")
  (test-one-fixture "r'hello\\\"'")

  ;;; raw docstrings
  (test-one-fixture "r'''hello world'''")
  (test-one-fixture "r\"\"\"hello world\"\"\"")

  (test-one-fixture "r'''hello \\' world'''")
  (test-one-fixture "r'''hello \\\" world'''")
  (test-one-fixture "r\"\"\"hello \\' world\"\"\"")
  (test-one-fixture "r\"\"\"hello \\\" world\"\"\"")

  (test-one-fixture "r'''hello world\\''''")
  (test-one-fixture "r'''hello world\\\"'''")
  (test-one-fixture "r\"\"\"hello world\\\"\"\"\"")
  (test-one-fixture "r\"\"\"hello world\\'\"\"\"")

  ;;; like a real case
  (test-one-fixture "'''
Simple doctest for digits

>>> 2 * 2
4
>>> 3 * 3
9
'''")

  (test-one-fixture "'''
Strings with backslashes

>>> path = 'C:\\\\\\\\autoexec.bat'
>>> path
'C:\\\\\\\\autoexec.bat'
>>> print(path)
C:\\\\autoexec.bat
'''")

  (test-one-fixture "r'''
Strings with backslashes

>>> path = 'C:\\\\autoexec.bat'
>>> path
'C:\\\\autoexec.bat'
>>> print(path)
C:\\autoexec.bat
'''"))
