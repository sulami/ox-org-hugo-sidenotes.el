(require 'ert)
(require 'ox)
(require 'ox-org-hugo-sidenotes)

(defun convert (string &optional options)
  (org-export-string-as string
                        'org-hugo-sidenotes
                        nil
                        options))

(ert-deftest org-hugo-sidenotes-test/identity ()
  (let ((input "* Headline\n"))
    (should (equal input
                   (convert input
                            '(:with-title nil
                              :with-author nil
                              :ox-org-hugo-sidenotes-add-date nil))))))
