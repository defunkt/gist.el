;; gist.el --- Emacs integration for gist.github.com

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Chris Wanstrath <chris@ozmm.org>
;; Contributors:
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Dan McKinley
;; Yann Hodique <yann.hodique@gmail.com>
;; Version: 0.5
;; Created: 21 Jul 2008
;; Keywords: gist git github paste pastie pastebin

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Uses your local GitHub config if it can find it.
;; See http://github.com/blog/180-local-github-config

;; THIS IS AN EXPERIMENTAL VERSION BASED ON gh.el LIBRARY
;; see https://github.com/sigma/gh.el

;;; Code:

(require 'gh-gist)

(defvar gist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after
they're posted.")

(defvar gist-supported-modes-alist '((action-script-mode . "as")
                                     (c-mode . "c")
                                     (c++-mode . "cpp")
                                     (clojure-mode . "clj")
                                     (common-lisp-mode . "lisp")
                                     (css-mode . "css")
                                     (diff-mode . "diff")
                                     (emacs-lisp-mode . "el")
                                     (erlang-mode . "erl")
                                     (haskell-mode . "hs")
                                     (html-mode . "html")
                                     (io-mode . "io")
                                     (java-mode . "java")
                                     (javascript-mode . "js")
                                     (jde-mode . "java")
                                     (js2-mode . "js")
                                     (lua-mode . "lua")
                                     (ocaml-mode . "ml")
                                     (objective-c-mode . "m")
                                     (perl-mode . "pl")
                                     (php-mode . "php")
                                     (python-mode . "py")
                                     (ruby-mode . "rb")
                                     (text-mode . "txt")
                                     (scala-mode . "scala")
                                     (sql-mode . "sql")
                                     (scheme-mode . "scm")
                                     (smalltalk-mode . "st")
                                     (sh-mode . "sh")
                                     (tcl-mode . "tcl")
                                     (tex-mode . "tex")
                                     (xml-mode . "xml")))

;;;###autoload
(defun gist-region (begin end &optional private callback)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                  (file-name-extension file)
                  "txt"))
         (api (gh-gist-api "api" :sync nil))
         (filename (concat (file-name-sans-extension name) "." ext)) 
         (files (list (gh-gist-gist-file "file" 
                                         :filename filename
                                         :content (buffer-substring begin end))))
         (gist (gh-gist-gist "gist" 
                             :public (not private)
                             :description "posted from gist.el"
                             :files files)))
    (let ((resp (gh-gist-new api gist)))
      (gh-api-add-response-callback resp (or callback 'gist-created-callback)))))

(defun gist-created-callback (gist)
  (let ((location (oref gist :url)))
    (message "Paste created: %s" location)
    (when gist-view-gist
      (browse-url location))
    (kill-new location)))

;;;###autoload
(defun gist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region begin end t))

;;;###autoload
(defun gist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (gist-region (point-min) (point-max) private))

;;;###autoload
(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region-private (point-min) (point-max)))

;;;###autoload
(defun gist-region-or-buffer (&optional private)
  "Post either the current region, or if mark is not set, the current buffer as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (condition-case nil
      (gist-region (point) (mark) private)
      (mark-inactive (gist-buffer private))))

;;;###autoload
(defun gist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the current buffer as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive)
  (condition-case nil
      (gist-region-private (point) (mark))
      (mark-inactive (gist-buffer-private))))

;;;###autoload
(defun gist-list ()
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (let ((api (gh-gist-api "api" :sync nil)))
    (let ((resp (gh-gist-list api)))
      (gh-api-add-response-callback 
       resp 'gist-lists-retrieved-callback))))

(defun gist-tabulated-entry (gist)
  (let* ((data (gist-parse-gist gist))
         (repo (car data)))
    (list repo (apply 'vector data))))

(defun gist-lists-retrieved-callback (gists)
  "Called when the list of gists has been retrieved. Displays
the list."
  (with-current-buffer (get-buffer-create "*gists*")
    (gist-list-mode)
    (setq tabulated-list-entries
          (mapcar 'gist-tabulated-entry gists))
    (tabulated-list-print)
    (set-window-buffer nil (current-buffer))))

(defun gist-parse-gist (gist)
  "Returns a list of the gist's attributes for display, given the xml list
for the gist."
  (let ((repo (oref gist :id))
        (created-at (let ((vec (timezone-parse-date (oref gist :date))))
                      (format "%s-%s-%s %s" 
                              (aref vec 0) (aref vec 1) (aref vec 2) (aref vec 3))))
        (description (or (oref gist :description) ""))
        (public (if (eq t (oref gist :public)) "public" "private")))
    (list repo created-at public description)))

;;;###autoload
(defun gist-fetch (id)
  "Fetches a Gist and inserts it into a new buffer
If the Gist already exists in a buffer, switches to it"
  (interactive "sGist ID: ")

  (let* ((gist-buffer-name (format "*gist %s*" id))
         (gist-buffer (get-buffer gist-buffer-name)))
    (if (bufferp gist-buffer)
        (switch-to-buffer-other-window gist-buffer)
      (message "Fetching Gist %s..." id)
      (let* ((gist-buffer (get-buffer-create gist-buffer-name))
             (api (gh-gist-api "api" :sync t))
             (gist (oref (gh-gist-get api id) :data))
             ;; TODO fix this, as it's obviously broken for multi-files gist
             (file (car (oref gist :files)))
             (mode (car (rassoc (file-name-extension (oref file :filename))
                                gist-supported-modes-alist))))
        (with-current-buffer gist-buffer
          (delete-region (point-min) (point-max))
          (insert (oref file :content))
          (when (fboundp mode)
            (funcall mode))
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window gist-buffer)))))

(defun gist-fetch-current ()
  (interactive)
  (gist-fetch (tabulated-list-get-id)))

(defvar gist-list-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'gist-fetch-current)
    map))

(define-derived-mode gist-list-mode tabulated-list-mode "Gist Menu"
  "Major mode for browsing gists.
\\<gist-list-menu-mode-map>
\\{gist-list-menu-mode-map}"
  (setq tabulated-list-format [("Id" 9 nil)
                               ("Created" 20 nil)
                               ("Visibility" 10 nil)
                               ("Description" 0 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (use-local-map gist-list-menu-mode-map))

(provide 'gist)
;;; gist.el ends here.
