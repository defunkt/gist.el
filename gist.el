;;; gist.el --- Emacs integration for gist.github.com -*- lexical-binding: t; -*-

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; URL: https://github.com/defunkt/gist.el
;; Original Author: Christian Neukirchen <chneukirchen@gmail.com>
;; Contributors: Chris Wanstrath <chris@ozmm.org>
;;               Will Farrington <wcfarrington@gmail.com>
;;               Michael Ivey
;;               Phil Hagelberg
;;               Dan McKinley
;;               Marcelo Muñoz Araya <ma.munoz.araya@gmail.com>
;; Version: 1.4.0
;; Package-Requires: ((emacs "24.3") (gh "0.10.0"))
;; Keywords: tools
;; Homepage: https://github.com/defunkt/gist.el

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

;; An Emacs interface for managing gists (http://gist.github.com).

;;; Code:


(require 'cl-lib)
(require 'eieio)
(require 'eieio-base)
(require 'timezone)
(require 'gh-api)
(require 'gh-gist)
(require 'gh-profile)
(require 'tabulated-list)

(declare-function dired-get-marked-files "dired")

(defgroup gist nil
  "Interface to GitHub's Gist."
  :group 'applications)

(defcustom gist-list-format '((id "Id" 10 nil identity)
                              (created "Created" 20 nil "%D %R")
                              (visibility "Visibility" 10 nil
                                          (lambda (public)
                                            (or (and public "public")
                                                "private")))
                              (description "Description" 0 nil identity))
  "Format for gist list."
  :type '(alist :key-type
          (choice
           (const :tag "Id" id)
           (const :tag "Creation date" created)
           (const :tag "Visibility" visibility)
           (const :tag "Description" description)
           (const :tag "Files" files))
          :value-type
          (list
           (string :tag "Label")
           (integer :tag "Field length")
           (boolean :tag "Sortable")
           (choice
            (string :tag "Format")
            (function :tag "Formatter"))))
  :group 'gist)

(defcustom gist-view-gist nil
  "If non-nil, view gists with `browse-url' after posting."
  :type 'boolean
  :group 'gist)

(defcustom gist-multiple-files-mark "+"
  "Symbol to use to indicate gists with multiple files."
  :type 'string
  :group 'gist)

(defcustom gist-ask-for-description nil
  "If non-nil, prompt for description before submitting gist."
  :type 'boolean
  :group 'gist)

(defcustom gist-ask-for-filename nil
  "If non-nil, prompt for change default file name before submitting gist."
  :type 'boolean
  :group 'gist)

(defcustom gist-created-fmt "Paste created: %s"
  "Format for the message to show after successful gist creation.
Must contain a single %s for the location of the newly
created gist."
  :type 'string
  :group 'gist)

(defcustom gist-supported-modes-alist '((action-script-mode . "as")
                                        (c-mode . "c")
                                        (c++-mode . "cpp")
                                        (clojure-mode . "clj")
                                        (common-lisp-mode . "lisp")
                                        (css-mode . "css")
                                        (diff-mode . "diff")
                                        (emacs-lisp-mode . "el")
                                        (lisp-interaction-mode . "el")
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
                                        (xml-mode . "xml"))
  "Mapping between major-modes and file extensions.
Used to generate filenames for created gists, and to select
appropriate modes from fetched gist files (based on filenames)."
  :type '(alist :key-type   (symbol :tag "Mode")
                :value-type (string :tag "Extension")))

(defvar gist-list-db nil)
(unless (hash-table-p gist-list-db)
  (setq gist-list-db (make-hash-table :test 'equal)))

(defvar gist-list-db-by-user nil)
(unless (hash-table-p gist-list-db-by-user)
  (setq gist-list-db-by-user (make-hash-table :test 'equal)))

(defvar gist-list-limits nil)

(defvar gist-id nil)
(make-variable-buffer-local 'gist-id)

(defvar gist-filename nil)
(make-variable-buffer-local 'gist-filename)

(defvar gist-user-history nil "History list for `gist-list-user'.")
(defvar gist-list-buffer-user nil "Username for this gist buffer.")
(make-variable-buffer-local 'gist-list-buffer-user)
(put 'gist-list-buffer-user 'permanent-local t)

(defun gist-get-api (&optional sync)
  "Reuse or create new gist api.
If SYNC is non-nil, make requests synchronously."
  (let ((gh-profile-current-profile
         (or gh-profile-current-profile (gh-profile-completing-read))))
    (make-instance 'gh-gist-api
                   :sync sync
                   :cache t
                   :num-retries 1)))

(defun gist-internal-new (files &optional private description callback)
  "Post new gist with FILES and DESCRIPTION.
With a prefix argument PRIVATE, makes a PRIVATE paste.
Invoke function CALLBACK with created gist as argument."
  (let* ((api (gist-get-api))
         (gist (make-instance 'gh-gist-gist-stub
                              :public (or (not private) json-false)
                              :description (or description "")
                              :files files))
         (resp (gh-gist-new api gist)))
    (gh-url-add-response-callback
     resp
     (let ((profile (oref api profile))
           (cb callback))
       (lambda (gist)
         (let ((gh-profile-current-profile profile))
           (funcall (or cb 'gist-created-callback) gist)))))))

(defun gist-ask-for-description-maybe ()
  "Read a string from the minibuffer if `gist-ask-for-description' is non nil."
  (when gist-ask-for-description
    (read-from-minibuffer "Gist description: ")))

(defun gist-ask-for-filename-maybe (fname)
  "Read a name for gist the minibuffer If `gist-ask-for-filename' is non-nil.
FNAME is as default name for gist."
  (if gist-ask-for-filename
      (read-string (format "File name (%s): " fname) nil nil fname)
    fname))

;;;###autoload
(defun gist-region (begin end &optional private callback)
  "Post the current region between BEGIN and END as a new paste.
Copies the URL into the kill ring.

With a prefix argument PRIVATE, makes a private paste.
Invoke function CALLBACK with created gist as argument."
  (interactive "r\nP")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                  (file-name-extension file)
                  "txt"))
         (proposal-fname (concat (file-name-sans-extension name) "." ext))
         (fname (gist-ask-for-filename-maybe proposal-fname))
         (files (list
                 (make-instance 'gh-gist-gist-file
                                :filename fname
                                :content (buffer-substring begin end)))))
    (gist-internal-new files private
                       (gist-ask-for-description-maybe) callback)))

(defun gist-files (filenames &optional private callback)
  "Create gist from existing FILENAMES and invoke CALLBACK function.
If PRIVATE is non-nil, create private gists."
  (let ((files nil))
    (dolist (f filenames)
      (with-temp-buffer
        (insert-file-contents f)
        (let ((name (file-name-nondirectory f)))
          (push (make-instance 'gh-gist-gist-file
                               :filename name
                               :content (buffer-string))
                files))))
    (gist-internal-new files private
                       (gist-ask-for-description-maybe) callback)))

(defun gist-created-callback (gist)
  "Reload gists and copy url of new GIST."
  (let ((location (oref gist html-url)))
    (gist-list-reload 'current-user t)
    (message gist-created-fmt location)
    (when gist-view-gist
      (browse-url location))
    (kill-new location)))

;;;###autoload
(defun gist-region-private (begin end)
  "Post the current region between BEGIN and END as a new private paste.
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region begin end t))

;;;###autoload
(defun gist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument PRIVATE, makes a private paste."
  (interactive "P")
  (gist-region (point-min)
               (point-max) private))

;;;###autoload
(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region-private (point-min) (point-max)))

;;;###autoload
(defun gist-region-or-buffer (&optional private)
  "Post either the current region or the current buffer at at gist.github.com.

Copies the URL into the kill ring.

With a prefix argument, makes a PRIVATE paste."
  (interactive "P")
  (if (region-active-p)
      (gist-region (point) (mark) private)
    (gist-buffer private)))

;;;###autoload
(defun gist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the current buffer.

Copies the URL into the kill ring."
  (interactive)
  (if (region-active-p)
      (gist-region-private (point) (mark))
    (gist-buffer-private)))

;;;###autoload
(defun gist-list-user (username &optional force-reload background)
  "Displays a list of gists of user USERNAME in a new buffer.

If BACKGROUND is non-nil, don't show buffer.

When called from a program, pass ='current-user as the username to view
the user's own gists, or nil for the username and a non-nil value for
FORCE-RELOAD to reload the gists for the current buffer."
  (interactive
   (let ((username (read-from-minibuffer "GitHub user: " nil nil nil
                                         'gist-user-history))
         (force-reload (equal current-prefix-arg '(4))))
     (list username force-reload)))
  ;; if buffer exists, it contains the current gh profile
  (let* ((gh-profile-current-profile (or gh-profile-current-profile
                                         (gh-profile-completing-read)))
         (bufname (if (null username)
                      (if (not (equal major-mode 'gist-list-mode))
                          (error "Current buffer isn't a gist-list-mode buffer")
                        (buffer-name))
                    (format "*%s:%sgists*"
                            gh-profile-current-profile
                            (if (or (equal "" username)
                                    (eq 'current-user username))
                                ""
                              (format "%s's-" username)))))
         (api (gist-get-api nil))
         (username (or (and (null username) gist-list-buffer-user)
                       (and (not (or (null username)
                                     (equal "" username)
                                     (eq 'current-user username)))
                            username)
                       (gh-api-get-username api))))
    (when force-reload
      (pcache-clear (oref api cache))
      (or background (message "Retrieving list of gists...")))
    (unless (and background (not (get-buffer bufname)))
      (let ((resp (gh-gist-list api username)))
        (gh-url-add-response-callback
         resp
         (let ((buffer bufname))
           (lambda (gists)
             (with-current-buffer (get-buffer-create buffer)
               (setq gist-list-buffer-user username)
               (gist-lists-retrieved-callback gists background)))))
        (gh-url-add-response-callback
         resp
         (let ((profile (oref api profile))
               (buffer bufname))
           (lambda (&rest _args)
             (with-current-buffer buffer
               (setq gh-profile-current-profile profile)))))))))

;;;###autoload
(defun gist-list (&optional force-reload background)
  "Display a list of all of the current user's gists in a new buffer.
Clear caches if FORCE-RELOAD is non-nil.
If BACKGROUND is non-nil, don't show it's buffer."
  (interactive "P")
  (gist-list-user 'current-user force-reload background))


(defun gist-list-reload (&optional username background)
  "Clear caches and reload gists of user with USERNAME.
If BACKGROUND is non-nil, don't show it's buffer."
  (interactive)
  (gist-list-user username t background))

(defun gist-list-redisplay ()
  "Redisplay a list of current user gists."
  (gist-list-user 'current-user))

(defun gist-tabulated-entry (gist)
  "Make tabulated entry from GIST."
  (let* ((data (gist-parse-gist gist))
         (repo (oref gist id)))
    (list repo (apply #'vector data))))

(defun gist-lists-retrieved-callback (gists &optional background)
  "Display list of retrieved GISTS.
If BACKGROUND is non-nil, don't show it's buffer."
  (dolist (g (gethash gist-list-buffer-user gist-list-db-by-user))
    (remhash (oref g id) gist-list-db))
  (dolist (g gists)
    (puthash (oref g id) g gist-list-db))
  (puthash gist-list-buffer-user gists gist-list-db-by-user)
  (gist-list-render (gethash gist-list-buffer-user gist-list-db-by-user)
                    background))

(defun gist--get-time (gist)
  "Return timestamp from GIST."
  (let* ((date (timezone-parse-date (oref gist date)))
         (time (timezone-parse-time (aref date 3))))
    (encode-time (string-to-number (aref time 2))
                 (string-to-number (aref time 1))
                 (string-to-number (aref time 0))
                 (string-to-number (aref date 2))
                 (string-to-number (aref date 1))
                 (string-to-number (aref date 0))
                 (aref date 4))))

(defun gist-parse-gist (gist)
  "Return a list of the GIST's attributes for display.
See also the variable `gist-list-format'."
  (let ((repo (oref gist id))
        (creation (gist--get-time gist))
        (desc (or (oref gist description) ""))
        (public (eq t (oref gist public)))
        (fnames (mapcar (lambda (f)
                          (when f (oref f filename)))
                        (oref gist files))))
    (cl-loop for (id _label _width _sort format) in gist-list-format
             collect (let ((string-formatter (if (eq id 'created)
                                                 'format-time-string
                                               'format))
                           (value (cond ((eq id 'id) repo)
                                        ((eq id 'created) creation)
                                        ((eq id 'visibility) public)
                                        ((eq id 'description) desc)
                                        ((eq id 'files) fnames))))
                       (funcall (if (stringp format)
                                    (lambda (val)
                                      (funcall string-formatter format val))
                                  format)
                                value)))))

;;;###autoload
(defun gist-fetch (id)
  "Fetch gist with ID."
  (interactive "sGist ID: ")
  (require 'ibuffer)
  (let ((gist nil)
        (multi nil)
        (prefix (format "*gist-%s*" id))
        (result nil)
        (profile (gh-profile-current-profile)))
    (setq gist (gist-list-db-get-gist id))
    (let ((api (gist-get-api t)))
      (cond ((null gist)
             ;; fetch it
             (setq gist (oref (gh-gist-get api id) data))
             (puthash (oref gist id) gist gist-list-db)
             (let* ((user (oref gist user))
                    (gists (push gist (gethash user gist-list-db-by-user))))
               (puthash user gists gist-list-db-by-user)))
            ((not (gh-gist-gist-has-files gist))
             (gh-gist-get api gist))))
    (let ((files (oref gist files)))
      (setq multi (< 1 (length files)))
      (dolist (f files)
        (let ((buffer (get-buffer-create (format "%s/%s" prefix
                                                 (oref f filename))))
              (mode (car (rassoc (file-name-extension (oref f filename))
                                 gist-supported-modes-alist))))
          (with-current-buffer buffer
            (delete-region (point-min)
                           (point-max))
            (insert (oref f content))
            (let ((fname (oref f filename)))
              ;; set major mode
              (if (fboundp mode)
                  (funcall mode)
                (let ((buffer-file-name fname)
                      enable-dir-local-variables)
                  (normal-mode)))
              ;; set minor mode
              (gist-mode 1)
              (setq gist-id id
                    gist-filename fname
                    gh-profile-current-profile profile))
            (set-buffer-modified-p nil))
          (setq result buffer))))
    (if multi
        (ibuffer t prefix
                 `((name . ,(regexp-quote (concat prefix "/"))))
                 nil nil
                 nil
                 '((name)))
      (switch-to-buffer-other-window result))))

(defun gist-fetch-current ()
  "Fetch gist at point and switch to it's buffer."
  (interactive)
  (gist-fetch (tabulated-list-get-id)))


(defun gist-fetch-current-noselect ()
  "Display content of a gist at point without switching to it's buffer."
  (interactive)
  (let ((win (selected-window)))
    (gist-fetch-current)
    (select-window win)))

(defun gist--check-perms-and-get-api (gist errormsg apiflg)
  "Signal user error ERRORMSG if GIST is not present in `gist-list-db-by-user'."
  (let* ((api (gist-get-api apiflg))
         (username (gh-api-get-username api))
         (gs (gethash username gist-list-db-by-user)))
    (if (not (memq gist gs))
        (user-error errormsg)
      api)))

(defun gist-edit-current-description ()
  "Edit description for gist at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id))
         (api (gist--check-perms-and-get-api
               gist "Can't edit a gist that doesn't belong to you" t)))
    (let* ((old-descr (oref gist description))
           (new-descr (read-from-minibuffer "Description: " old-descr))
           (g (clone gist
                     :files nil
                     :description new-descr))
           (resp (gh-gist-edit api g)))
      (gh-url-add-response-callback resp
                                    (lambda (_gist)
                                      (gist-list-reload))))))

(defun gist-add-buffer (buffer)
  "Add BUFFER to the gist at point."
  (interactive "bBuffer: ")
  (let* ((buffer (get-buffer buffer))
         (id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id))
         (api (gist--check-perms-and-get-api
               gist "Can't modify a gist that doesn't belong to you" t))
         (fname (file-name-nondirectory (or (buffer-file-name buffer)
                                            (buffer-name buffer))))
         (g (clone gist :files
                   (list
                    (make-instance 'gh-gist-gist-file
                                   :filename fname
                                   :content (with-current-buffer buffer
                                              (buffer-string))))))
         (resp (gh-gist-edit api g)))
    (gh-url-add-response-callback resp
                                  (lambda (_gist)
                                    (gist-list-reload)))))


(defun gist-remove-file (fname)
  "Remove gist's file FNAME."
  (interactive (list
                (completing-read
                 "Filename to remove: "
                 (let* ((id (tabulated-list-get-id))
                        (gist (gist-list-db-get-gist id)))
                   (mapcar #'(lambda (f)
                               (oref f filename))
                           (oref gist files))))))
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id))
         (api (gist--check-perms-and-get-api
               gist "Can't modify a gist that doesn't belong to you" t))
         (g (clone gist :files
                   (list
                    (make-instance 'gh-gist-gist-file
                                   :filename fname
                                   :content nil))))
         (resp (gh-gist-edit api g)))
    (gh-url-add-response-callback resp
                                  (lambda (_gist)
                                    (gist-list-reload)))))

(defun gist-kill-current ()
  "Delete gist at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (gist (gist-list-db-get-gist id))
         (api (gist--check-perms-and-get-api
               gist "Can't delete a gist that doesn't belong to you" t)))
    (when (yes-or-no-p (format "Really delete gist %s ? " id))
      (gh-gist-delete api id)
      (gist-list-reload))))

(defun gist-current-url ()
  "Helper function to fetch current gist url."
  (let* ((id (or (and (eq major-mode 'gist-list-mode)
                      (tabulated-list-get-id))
                 (and (boundp 'gist-mode)
                      gist-mode
                      gist-id)))
         (gist (gist-list-db-get-gist id)))
    (oref gist html-url)))

(defun gist-print-current-url ()
  "Display and copy the currently selected gist's url in the echo area."
  (interactive)
  (kill-new (message (gist-current-url))))


(defun gist-browse-current-url ()
  "Browse current gist on github."
  (interactive)
  (browse-url (gist-current-url)))

(defun gist--do-star (id how msg)
  "Star (if HOW is non-nil) or unstar gist with ID and show message MSG."
  (let* ((api (gist-get-api t))
         (resp (gh-gist-set-star api id how)))
    (gh-url-add-response-callback resp
                                  (lambda (_gist)
                                    (message msg id)))))

;;;###autoload
(defun gist-star ()
  "Star gist at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gist--do-star id t "Starred gist %s")))

;;;###autoload
(defun gist-unstar ()
  "Unstar gist at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (gist--do-star id nil "Unstarred gist %s")))

;;;###autoload
(defun gist-list-starred (&optional background)
  "List your starred gists.
If BACKGROUND is non nil, don't show the buffer."
  (interactive)
  (let* ((api (gist-get-api t))
         (resp (gh-gist-list-starred api)))
    (gh-url-add-response-callback
     resp
     (let ((buffer "*starred-gists*"))
       (lambda (gists)
         (with-current-buffer (get-buffer-create buffer)
           (gist-list-render gists background)))))))

;;;###autoload
(defun gist-fork ()
  "Fork a gist."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (api (gist-get-api))
         (resp (gh-gist-fork api id)))
    (gh-url-add-response-callback resp
                                  (lambda (_gist)
                                    (message "Forked gist %s" id)))))

(defvar gist-list-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'gist-fetch-current)
    (define-key map [tab] 'gist-fetch-current-noselect)
    (define-key map "g" 'gist-list-reload)
    (define-key map "e" 'gist-edit-current-description)
    (define-key map "k" 'gist-kill-current)
    (define-key map "+" 'gist-add-buffer)
    (define-key map "-" 'gist-remove-file)
    (define-key map "y" 'gist-print-current-url)
    (define-key map "b" 'gist-browse-current-url)
    (define-key map "*" 'gist-star)
    (define-key map "^" 'gist-unstar)
    (define-key map "f" 'gist-fork)
    (define-key map "/p" 'gist-list-push-visibility-limit)
    (define-key map "/t" 'gist-list-push-tag-limit)
    (define-key map "/w" 'gist-list-pop-limit)
    map))

(define-derived-mode gist-list-mode tabulated-list-mode "Gists"
  "Major mode for browsing gists.
\\<gist-list-menu-mode-map>
\\{gist-list-menu-mode-map}"
  (setq tabulated-list-format
        (apply #'vector
               (cl-loop for (_sym label width sort _format) in gist-list-format
                     collect (list label width sort)))
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (use-local-map gist-list-menu-mode-map)
  (font-lock-add-keywords nil '(("#[^[:space:]]*" . 'font-lock-keyword-face))))


(defun gist-list-pop-limit (&optional all)
  "Toggle visibility filter for gists.
With prefix argument ALL, show all gists, othervise pop latest filter."
  (interactive "P")
  (if all
      (setq gist-list-limits nil)
    (pop gist-list-limits))
  (gist-list-redisplay))


(defun gist-list-push-visibility-limit (&optional private)
  "Push visibility filter.
If PRIVATE is non nil, show public gists, othervise private."
  (interactive "P")
  (push (apply-partially (lambda (flag g)
                           (or (and flag (not (oref g public)))
                               (and (not flag)
                                    (oref g public))))
                         private)
        gist-list-limits)
  (gist-list-redisplay))

(defun gist-parse-tags (tags)
  "Parse TAGS."
  (let ((words (split-string tags))
        with without)
    (dolist (w words)
      (cond ((string-prefix-p "+" w)
             (push (substring w 1) with))
            ((string-prefix-p "-" w)
             (push (substring w 1) without))
            (t
             (push w with))))
    (list with without)))

(defun gist-list-push-tag-limit (tags)
  "Push TAGS limit."
  (interactive "sTags: ")
  (let* ((lsts (gist-parse-tags tags))
         (with (car lsts))
         (without (cadr lsts)))
    (push (apply-partially (lambda (with without g)
                             (and
                              (cl-every (lambda (tag)
                                          (string-match-p
                                           (format "#%s\\>" tag)
                                           (oref g description)))
                                        with)
                              (not (cl-some (lambda (tag)
                                              (string-match-p
                                               (format "#%s\\>" tag)
                                               (oref g description)))
                                            without))))
                           with without)
          gist-list-limits))
  (gist-list-redisplay))

(defun gist-list-apply-limits (gists)
  "Apply filter to GISTS."
  (condition-case nil
      (delete nil
              (mapcar
               (lambda (g)
                 (when (cl-every #'identity
                                 (mapcar (lambda (f)
                                           (funcall f g))
                                         gist-list-limits))
                   g))
               gists))
    (error gists)))

(defun gist-list-render (gists &optional background)
  "Render list of GISTS.
If BACKGROUND is non-nil, don't show it's buffer."
  (gist-list-mode)
  (let ((entries (mapcar #'gist-tabulated-entry
                         (gist-list-apply-limits gists))))
    (setq tabulated-list-entries entries)
    (when (not (equal (length gists)
                      (length entries)))
      (setq mode-name (format "Gists[%d/%d]" (length entries)
                              (length gists)))))
  (tabulated-list-print)
  (gist-list-tag-multi-files)
  (unless background
    (set-window-buffer nil (current-buffer))))

(defun gist-list-tag-multi-files ()
  "Put tags to gists with multiple files in the padding area."
  (let ((ids nil))
    (maphash (lambda (_k v)
               (when (< 1 (length (oref v files)))
                 (push (oref v id) ids)))
             gist-list-db)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (member (tabulated-list-get-id) ids)
            (tabulated-list-put-tag gist-multiple-files-mark t)
          (forward-line 1))))))

(defun gist-list-db-get-gist (id)
  "Return gist with ID from `gist-list-db'."
  (gethash id gist-list-db))

;;; Gist minor mode
(defun gist-mode-edit-buffer (&optional new-name)
  "Save currently editing gist.
With NEW-NAME rename gist."
  (when (or (buffer-modified-p) new-name)
    (let* ((id gist-id)
           (gist (gist-list-db-get-gist id))
           (files (list
                   (make-instance 'gh-gist-gist-file
                                  :filename (or new-name gist-filename)
                                  :content (buffer-string)))))
      (when new-name
        ;; remove old file as well
        (push (make-instance 'gh-gist-gist-file
                             :filename gist-filename
                             :content nil)
              files))
      (let* ((g (clone gist
                       :files files))
             (api (gist-get-api t))
             (resp (gh-gist-edit api g)))
        (gh-url-add-response-callback
         resp
         (lambda (gist)
           (set-buffer-modified-p nil)
           (when new-name
             (rename-buffer (replace-regexp-in-string "/.*$"
                                                      (concat "/" new-name)
                                                      (buffer-name)))
             (setq gist-filename new-name))
           (let ((g (gist-list-db-get-gist (oref gist id))))
             (oset g :files (oref gist files)))))))))

(defun gist-mode-save-buffer ()
  "Save gist in edit buffer."
  (interactive)
  (gist-mode-edit-buffer))

(defun gist-mode-write-file ()
  "Rename current gist."
  (interactive)
  (let ((new-name (read-from-minibuffer "File name: " gist-filename)))
    (gist-mode-edit-buffer new-name)))

(defvar gist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'gist-mode-save-buffer)
    (define-key map [remap write-file] 'gist-mode-write-file)
    map))

(define-minor-mode gist-mode
  "Minor mode for buffers containing gists files."
  :lighter " gist"
  :map 'gist-mode-map)

;;; Dired integration

(defun gist-dired-do-gist (&optional private)
  "Create gist from marked files in `dired'.
If PRIVATE is non-nil, create private gists."
  (interactive "P")
  (require 'dired)
  (gist-files (dired-get-marked-files) private))

(provide 'gist)
;;; gist.el ends here
