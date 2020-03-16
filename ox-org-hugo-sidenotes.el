;;; ox-org-hugo-sidenotes.el --- Org Tufte backend for Org Export Engine

;; Copyright (C) 2020 Robin Schroer

;; Author: Robin Schroer <sulami@peerwire.org>
;; Version: 0.1-SNAPSHOT
;; Keywords: outlines, hypermedia, wp
;; Homepage: https://github.com/sulami/ox-org-hugo-sidenotes.el
;; Package-Requires: ((org "8.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adapts outputs modified org-mode to work with Hugo
;; sidenotes. It inlines footnotes wrapped in {{< sidenote >}}, and
;; removes the footnote definitions.

;; A lot of this code has been cargo-culted from
;; https://github.com/stig/ox-jira.el/blob/master/ox-jira.el.

(require 'ox)
(require 'ox-org)
(require 'rx)

;;; Code:

(defgroup org-hugo-sidenotes-export nil
  "Options specific to the org-hugo-sidenotes export backend."
  :tag "Org Export Hugo Sidenotes"
  :group 'org-export
  :version "25.2"
  :package-version '(ox-org-hugo-sidenotes . "0.1"))

(defcustom ox-org-hugo-sidenotes-add-current-date t
  "Automatically add the date header."
  :tag "Add date header to org-hugo-sidenotes exports"
  :group 'org-hugo-sidenotes-export
  :type 'boolean)

(defcustom ox-org-hugo-sidenotes-export-path ""
  "Where to export files to."
  :tag "org-hugo-sidenotes export path"
  :group 'org-hugo-sidenotes-export
  :type 'directory)

(org-export-define-derived-backend 'org-hugo-sidenotes 'org
  :translate-alist '((footnote-reference . ox-org-hugo-sidenotes-footnote-reference)
                     (link . ox-org-hugo-sidenotes-link)
                     (section . org-org-identity)
                     (template . ox-org-hugo-sidenotes-template))
  :options-alist '((:add-current-date nil nil ox-org-hugo-sidenotes-add-current-date)
                   (:export-path nil nil ox-org-hugo-sidenotes-export-path))
  :menu-entry
  '(?O "Export to Org-sidenotes"
       ((?s "As Tufte file" ox-org-hugo-sidenotes-export-to-file)
        (?S "As Tufte buffer" ox-org-hugo-sidenotes-export-to-buffer))))

(defun ox-org-hugo-sidenotes--timestamp ()
  "Return a YYYY-MM-DD timestamp of the current date."
  (format-time-string "%Y-%m-%d" (current-time)))

(defun ox-org-hugo-sidenotes-link
    (link contents info)
  "Convert file links to relative Hugo links."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (if (equal "file" type)
        (format "[[{{< ref \"%s\" >}}][%s]]"
                path
                contents)
      (org-org-link link contents info))))

(defun ox-org-hugo-sidenotes-template
    (contents info)
    "Insert the current date as timestamp and removes the creation
timestamp from the file."
  (concat
   (when (plist-get info :add-current-date)
     (format "#+DATE: %s\n" (ox-org-hugo-sidenotes--timestamp)))
   (org-org-template contents (plist-put info :time-stamp-file nil))))

(defun ox-org-hugo-sidenotes-footnote-reference
    (footnote-reference contents info)
  "Insert the footnote definition in place using the sidenote
shortcode."
  (format "{{< sidenote id=\"%s\" >}}%s{{</ sidenote >}}"
          (org-export-get-footnote-number footnote-reference info)
          (let ((paragraph (org-export-get-footnote-definition footnote-reference info)))
            (org-trim (org-export-data paragraph info)))))

;;;###autoload
(defun ox-org-hugo-sidenotes-export-to-buffer
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as Tufte Org buffer.

If narrowing is active in the current buffer, only export its narrowed
part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously. The resulting buffer should be accessible through the
`org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree at
point, extracting information from the headline properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export contents
of hidden elements.

When optional argument BODY-ONLY is non-nil, omit header stuff. (e.g.
AUTHOR and TITLE.)

EXT-PLIST, when provided, is a property list with external parameters
overriding Org default settings, but still inferior to file-local
settings."
  (interactive)
  (org-export-to-buffer 'org-hugo-sidenotes "*Org Tufte Export*"
    async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun ox-org-hugo-sidenotes-export-to-file
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as Tufte Org file.

If narrowing is active in the current buffer, only export its narrowed
part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously. The resulting buffer should be accessible through the
`org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree at
point, extracting information from the headline properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export contents
of hidden elements.

When optional argument BODY-ONLY is non-nil, omit header stuff. (e.g.
AUTHOR and TITLE.)

EXT-PLIST, when provided, is a property list with external parameters
overriding Org default settings, but still inferior to file-local
settings."
  (interactive)
  (if (s-blank? ox-org-hugo-sidenotes-export-path)
      (error "Required setting ox-org-hugo-sidenotes-export-path is not set")
    (org-export-to-file 'org-hugo-sidenotes
        (format "%s/%s.org"
                ox-org-hugo-sidenotes-export-path
                (let ((title (car (plist-get (org-export-get-environment) :title))))
                  (replace-regexp-in-string
                   (rx (one-or-more blank))
                   "-"
                   (downcase title))))
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-org-hugo-sidenotes)

;;; ox-org-hugo-sidenotes.el ends here
