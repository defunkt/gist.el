;;; gist.el --- Emacs integration for gist.github.com

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Chris Wanstrath <chris@ozmm.org>
;; Contributors:
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Dan McKinley
;; Yann Hodique <yann.hodique@gmail.com>
;; Version: 0.5a
;; Created: 21 Jul 2008
;; Keywords: gist git github paste pastie pastebin
;; Package-Requires: ((gh "0.3.1") (tabulated-list "0"))

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

(eval-when-compile
  (require 'cl))

(require 'eieio)
(require 'eieio-base)
(require 'gh-gist)
(require 'tabulated-list)

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

(defun gist-internal-new (files &optional private description callback)
  (let* ((api (gh-gist-api "api" :sync nil))
         (gist (gh-gist-gist-stub "gist"
                                  :public (not private)
                                  :description (or description "")
                                  :files files))
         (resp (gh-gist-new api gist)))
    (gh-api-add-response-callback resp (or callback 'gist-created-callback))))

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
         (filename (concat (file-name-sans-extension name) "." ext))
         (files (list (gh-gist-gist-file "file"
                                         :filename filename
                                         :content (buffer-substring begin end)))))
    (gist-internal-new files private nil callback)))

(defun gist-files (filenames &optional private callback)
  (let ((files nil))
    (dolist (f filenames)
      (with-temp-buffer
        (insert-file-contents f)
        (let ((name (file-name-nondirectory f)))
          (push (gh-gist-gist-file name :filename name :content (buffer-string))
                files))))
    (gist-internal-new files private nil callback)))

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
(defun gist-list (&optional force-reload)
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive "P")
  (when (or force-reload
            (> (float-time (current-time))
               (+ (oref gist-list-cache-db :timestamp) gist-list-cache-timeout)))
    (gist-list-cache-invalidate gist-list-cache-db))
  (if (not (oref gist-list-cache-db :gists))
      (progn
        (message "Retrieving list of your gists...")
        (let ((api (gh-gist-api "api" :sync nil)))
          (let ((resp (gh-gist-list api)))
            (gh-api-add-response-callback
             resp 'gist-lists-retrieved-callback))))
    (gist-list-cache-render gist-list-cache-db)))

(defun gist-list-reload ()
  (interactive)
  (gist-list t))

(defun gist-tabulated-entry (gist)
  (let* ((data (gist-parse-gist gist))
         (repo (car data)))
    (list repo (apply 'vector data))))

(defun gist-lists-retrieved-callback (gists)
  "Called when the list of gists has been retrieved. Displays
the list."
  (gist-list-cache-update gist-list-cache-db gists)
  (gist-list-cache-render gist-list-cache-db))

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
  (interactive "sGist ID: ")
  (let ((gist nil)
        (multi nil)
        (prefix (format "*gist %s*" id))
        (result nil))
    (dolist (g (oref gist-list-cache-db :gists))
      (when (string= (oref g :id) id)
        (setq gist g)))
    (let ((api (gh-gist-api "api" :sync t)))
      (cond ((null gist)
             ;; fetch it
             (setq gist (oref (gh-gist-get api id) :data))
             (object-add-to-list gist-list-cache-db :gists gist))
            ((not (gh-gist-gist-has-files gist))
             (gh-gist-get api gist))))
    (let ((files (oref gist :files)))
      (setq multi (< 1 (length files)))
      (dolist (f files)
        (let ((buffer (get-buffer-create (format "%s/%s" prefix
                                                 (oref f :filename))))
              (mode (car (rassoc (file-name-extension (oref f :filename))
                                 gist-supported-modes-alist))))
          (with-current-buffer buffer
            (delete-region (point-min) (point-max))
            (insert (oref f :content))
            (when (fboundp mode)
              (funcall mode))
            ;; set minor mode
            (gist-mode 1)
            (setq gist-id id
                  gist-filename (oref f :filename))
            (set-buffer-modified-p nil))
          (setq result buffer))))
    (if multi
        (let ((ibuffer-mode-hook nil)
              (ibuffer-use-header-line nil)
              (ibuffer-show-empty-filter-groups nil))
          (ibuffer t prefix
                   `((name . ,(regexp-quote (concat prefix "/"))))
                   nil nil
                   nil
                   '((name))))
      (switch-to-buffer-other-window result))))

(defun gist-fetch-current ()
  (interactive)
  (gist-fetch (tabulated-list-get-id)))

(defun gist-edit-current-description ()
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-cache-get-gist gist-list-cache-db id))
         (old-descr (oref gist :description))
         (new-descr (read-from-minibuffer "Description: " old-descr)))
    (let* ((g (clone gist
                     :files nil
                     :description new-descr))
           (api (gh-gist-api "api" :sync t))
           (resp (gh-gist-edit api g)))
      (gh-api-add-response-callback resp
                                    (lambda (gist)
                                      (gist-list-reload))))))

(defun gist-kill-current ()
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (yes-or-no-p (format "Really delete gist %s ?" id) )
      (let* ((api (gh-gist-api "api" :sync t))
             (resp (gh-gist-delete api id)))
        (gist-list-reload)))))

(defvar gist-list-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'gist-fetch-current)
    (define-key map "g" 'gist-list-reload)
    (define-key map "e" 'gist-edit-current-description)
    (define-key map "k" 'gist-kill-current)
    map))

(define-derived-mode gist-list-mode tabulated-list-mode "Gist Menu"
  "Major mode for browsing gists.
\\<gist-list-menu-mode-map>
\\{gist-list-menu-mode-map}"
  (setq tabulated-list-format [("Id" 10 nil)
                               ("Created" 20 nil)
                               ("Visibility" 10 nil)
                               ("Description" 0 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (use-local-map gist-list-menu-mode-map))

;;; Gist list persistent cache

(defclass gist-list-cache (eieio-persistent)
  ((version :initarg :version
            :initform 0.1
            :type float)
   (timestamp :initarg :timestamp
              :initform (float-time (current-time))
              :type float)
   (gists :initarg :gists
          :initform nil
          :type list)))

(defvar gist-list-cache-file
  (concat user-emacs-directory "var/gists.eieio"))

(defun gist-list-cache-load ()
  (condition-case nil
      (eieio-persistent-read gist-list-cache-file)
    (error (gist-list-cache "Gists" :file gist-list-cache-file))))

(defun gist-list-cache-save ()
  (eieio-persistent-save gist-list-cache-db))

(defvar gist-list-cache-db (gist-list-cache-load))

(defvar gist-list-cache-timeout 600)

(defmethod gist-list-cache-render ((cache gist-list-cache))
  (let ((gists (oref cache :gists)))
    (with-current-buffer (get-buffer-create "*gists*")
      (gist-list-mode)
      (setq tabulated-list-entries
            (mapcar 'gist-tabulated-entry gists))
      (tabulated-list-print)
      (gist-list-cache-tag-multi-files cache)
      (set-window-buffer nil (current-buffer)))))

(defmethod gist-list-cache-tag-multi-files ((cache gist-list-cache))
  (let ((ids nil))
    (let ((gists (oref cache :gists)))
      (dolist (gist gists)
        (when (< 1 (length (oref gist :files)))
          (push (oref gist :id) ids))))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (if (member (tabulated-list-get-id) ids)
            (tabulated-list-put-tag "+" t)
          (forward-line 1))))))

(defmethod gist-list-cache-get-gist ((cache gist-list-cache) id)
  (let ((gists (oref cache :gists)))
    (loop for gist in gists if (string= (oref gist :id) id)
          return gist)))

(defmethod gist-list-cache-invalidate ((cache gist-list-cache))
  (oset cache :gists nil))

(defmethod gist-list-cache-update ((cache gist-list-cache) gists)
  (oset cache :gists gists)
  (oset cache :timestamp (float-time (current-time)))
  (gist-list-cache-save))

;;; Gist minor mode

(defvar gist-id nil)
(defvar gist-filename nil)

(defun gist-mode-edit-buffer (&optional new-name)
  (when (or (buffer-modified-p) new-name)
    (let* ((id gist-id)
           (gist (gist-list-cache-get-gist gist-list-cache-db id))
           (files (list
                   (make-instance 'gh-gist-gist-file
                                  :filename (or new-name gist-filename)
                                  :content (buffer-string)))))
      (when new-name
        ;; remove old file as well
        (add-to-list 'files
                     (make-instance 'gh-gist-gist-file
                                    :filename gist-filename
                                    :content nil)))
      (let* ((g (clone gist
                       :files files))
             (api (gh-gist-api "api" :sync t))
             (resp (gh-gist-edit api g)))
        (gh-api-add-response-callback
         resp
         (lambda (gist)
           (set-buffer-modified-p nil)
           (when new-name
             (rename-buffer (replace-regexp-in-string "/.*$"
                                                      (concat "/" new-name)
                                                      (buffer-name)))
             (setq gist-filename new-name))
           (let ((g (gist-list-cache-get-gist
                     gist-list-cache-db
                     (oref gist :id))))
             (oset g :files (oref gist :files)))))))))

(defun gist-mode-save-buffer ()
  (interactive)
  (gist-mode-edit-buffer))

(defun gist-mode-write-file ()
  (interactive)
  (let ((new-name (read-from-minibuffer "File name: " gist-filename)))
    (gist-mode-edit-buffer new-name)))

(defvar gist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'gist-mode-save-buffer)
    (define-key map [remap write-file] 'gist-mode-write-file)
    map))

(define-minor-mode gist-mode
  "Minor mode for buffers containing gists files"
  :lighter " gist"
  :map 'gist-mode-map
  (make-local-variable 'gist-id)
  (make-local-variable 'gist-filename))

;;; Dired integration

(require 'dired)

(defun dired-do-gist (&optional private)
  (interactive "P")
  (gist-files (dired-get-marked-files) private))

(define-key dired-mode-map "@" 'dired-do-gist)

(provide 'gist)
;;; gist.el ends here.
