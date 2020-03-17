(require 'ert)
(require 'ox)
(require 'ox-org-hugo-sidenotes)

(defvar-local default-options
  '(:with-title nil
    :with-author nil))

(defun convert (string &optional options)
  (org-export-string-as string
                        'org-hugo-sidenotes
                        nil
                        options))

(ert-deftest org-hugo-sidenotes-test/identity ()
  (let ((input "* Headline\n"))
    (should (equal input
                   (convert input
                            (append default-options
                                    '(:add-current-date nil)))))))

(ert-deftest org-hugo-sidenotes-test/add-date ()
  (let ((input "* Headline\n"))
    (should (equal (format "#+DATE: %s\n%s"
                           (ox-org-hugo-sidenotes--timestamp)
                           input)
                   (convert input
                            (append default-options
                                    '(:add-current-date t)))))))

(ert-deftest org-hugo-sidenotes-test/inline-sidenote ()
  (let* ((reference "[fn:1]")
         (footnote "Footnote text")
         (before (format "Some text%s\n\n%s %s" reference reference footnote))
         (after (format "Some text{{< sidenote id=\"1\" >}}%s{{</ sidenote >}}\n" footnote)))
    (should (equal after
                   (convert before
                            (append default-options
                                    '(:add-current-date nil)))))))

(ert-deftest org-hugo-sidenotes-test/sidenote-shortcode ()
  (let* ((shortcode "marginnote")
         (reference "[fn:1]")
         (footnote "Footnote text")
         (before (format "Some text%s\n\n%s %s" reference reference footnote))
         (after (format "Some text{{< %s id=\"1\" >}}%s{{</ %s >}}\n"
                        shortcode
                        footnote
                        shortcode)))
    (should (equal after
                   (convert before
                            (append default-options
                                    '(:add-current-date nil
                                      :sidenote-shortcode "marginnote")))))))

(ert-deftest org-hugo-sidenotes-test/disable-sidenotes ()
  (let* ((reference "[fn:1]")
         (footnote "Footnote text")
         (before (format "Some text%s\n\n%s %s\n" reference reference footnote)))
    (should (equal before
                   (convert before
                            (append default-options
                                    '(:add-current-date nil
                                      :use-sidenotes nil)))))))

(ert-deftest org-hugo-sidenotes-test/absolute-file-links ()
  (let* ((text "a link")
         (target "/foo/bar.org")
         (before (format "[[%s][%s]]" target text))
         (after (format "[[{{< ref \"%s\" >}}][%s]]\n" target text)))
    (should (equal after
                   (convert before
                            (append default-options
                                    '(:add-current-date nil)))))))

(ert-deftest org-hugo-sidenotes-test/relative-file-links ()
  (let* ((text "a link")
         (target "./foo/bar.org")
         (before (format "[[%s][%s]]" target text))
         (after (format "[[{{< ref \"%s\" >}}][%s]]\n" target text)))
    (should (equal after
                   (convert before
                            (append default-options
                                    '(:add-current-date nil)))))))

(ert-deftest org-hugo-sidenotes-test/file-file-links ()
  (let* ((text "a link")
         (target "bar.org")
         (before (format "[[file:%s][%s]]" target text))
         (after (format "[[{{< ref \"%s\" >}}][%s]]\n" target text)))
    (should (equal after
                   (convert before
                            (append default-options
                                    '(:add-current-date nil)))))))

(ert-deftest org-hugo-sidenotes-test/non-file-links ()
  (let* ((text "a link")
         (target "https://example.com")
         (before (format "[[%s][%s]]" target text))
         (after (format "[[%s][%s]]\n" target text)))
    (should (equal after
                   (convert before
                            (append default-options
                                    '(:add-current-date nil)))))))
