(load-file "calc-date-lib.el")

(ert-deftest test-add-days ()
  (should (equal '(:add-days 3)
		 (parse-command-line-arguments '("--add-days" "3")))))
