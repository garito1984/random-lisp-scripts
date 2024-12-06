(load-file "calc-date-lib.el")

(ert-deftest test-parse-command-line-arguments ()
  (should (equal '(:add-days 3)
		 (parse-command-line-arguments '("--add-days" "3"))))
  (should (equal `(:base-date ,(date-to-time "2024-12-25"))
		 (parse-command-line-arguments '("--base-date" "2024-12-25")))))

(ert-deftest test-days-in-seconds ()
  (let ((seconds-in-24h (* 60 60 24)))
    (should (equal (* seconds-in-24h 3)
		   (days-in-seconds 3)))))
