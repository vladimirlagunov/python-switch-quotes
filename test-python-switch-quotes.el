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


(defmacro test-one-fixture (fixture reversible)
  (remove
   nil
   `(let ((py-string) (py-string1) (py-string2))
      (setq py-string ,fixture)
      (setq py-string1 (switch-it py-string))
      (should (python-approves py-string py-string1))
      (setq py-string2 (switch-it py-string1))
      (should (python-approves py-string1 py-string2))
      ,(when reversible
         `(should (equal py-string py-string2))))))


(ert-deftest test-in-python ()
  ;;; simple strings
  (test-one-fixture "'hello world'" t)
  (test-one-fixture "\"hello world\"" t)  

  (test-one-fixture "'hello \\u0022 \\u0027 world\\n'" t)

  (test-one-fixture "'\\'hello world'" t)
  (test-one-fixture "'hello world\\''" t)
  (test-one-fixture "'hello \" world'" t)
  (test-one-fixture "'he\\'llo \\'\\'\\' world'" t)
  (test-one-fixture "\"he'llo ''' world\"" t)

  ;;; docstrings
  (test-one-fixture "'''hello world'''" t)
  (test-one-fixture "\"\"\"hello world\"\"\"" t)

  (test-one-fixture "'''hello 'quoted' world'''" t)
  (test-one-fixture "'''hello \"quoted\" world'''" t)

  (test-one-fixture "'''hello world\"'''" t)
  (test-one-fixture "\"\"\"hello world'\"\"\"" t)

  (test-one-fixture "'''\\'''hello \\''' world\\'\\'\\''''" t)
  (test-one-fixture "\"\"\"\\\"\"\"hello \\\"\"\" world\\\"\\\"\\\"\"\"\"" t)

  ;;; simple raw strings
  (test-one-fixture "r'hello world'" t)
  (test-one-fixture "R\"hello world\"" t)

  (test-one-fixture "r'hello \\' world'" t)
  (test-one-fixture "r\"hello \\' world\"" t)
  (test-one-fixture "r'hello \\\" world'" t)
  (test-one-fixture "r\"hello \\\" world\"" t)

  (test-one-fixture "r'\\'hello'" t)
  (test-one-fixture "r'hello\\''" t)
  (test-one-fixture "r'\\\"hello'" t)
  (test-one-fixture "r'hello\\\"'" t)

  ;;; raw docstrings
  (test-one-fixture "r'''hello world'''" t)
  (test-one-fixture "r\"\"\"hello world\"\"\"" t)

  (test-one-fixture "r'''hello \\' world'''" t)
  (test-one-fixture "r'''hello \\\" world'''" t)
  (test-one-fixture "r\"\"\"hello \\' world\"\"\"" t)
  (test-one-fixture "r\"\"\"hello \\\" world\"\"\"" t)

  (test-one-fixture "r'''hello world\\''''" t)
  (test-one-fixture "r'''hello world\\\"'''" t)
  (test-one-fixture "r\"\"\"hello world\\\"\"\"\"" t)
  (test-one-fixture "r\"\"\"hello world\\'\"\"\"" t)

  ;;; like a real case
  (test-one-fixture "'''
Simple doctest for digits

>>> 2 * 2
4
>>> 3 * 3
9
'''" t)

  (test-one-fixture "'''
Strings with backslashes

>>> path = 'C:\\\\\\\\autoexec.bat'
>>> path
'C:\\\\\\\\autoexec.bat'
>>> print(path)
C:\\\\autoexec.bat
'''" t)

  (test-one-fixture "r'''
Strings with backslashes

>>> path = 'C:\\\\autoexec.bat'
>>> path
'C:\\\\autoexec.bat'
>>> print(path)
C:\\autoexec.bat
'''" t))
