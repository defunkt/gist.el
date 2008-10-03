;; gist.el --- Emacs integration for gist.github.com
;; Copyright (C) 2008  Christian Neukirchen <purl.org/net/chneukirchen>
;; Copyright (C) 2008  Chris Wanstrath <chris@ozmm.org>
;; Copyright (C) 2008  Will Farrington <wcfarrington@gmail.com>
;; Licensed under the same terms as Emacs.

;; Version: 0.3.1
;; 26aug2008  +wfarr+
;; 25aug2008  +defunkt+
;; 21jul2008  +chris+

;; Ideas: fork

(defvar github-username "")
(defvar github-api-key "")

(defvar gist-supported-modes-alist '((action-script-mode . "as")
                                     (c-mode . "c")
                                     (c++-mode . "cpp")
                                     (common-lisp-mode . "el")
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
                                     (perl-mode "pl")
                                     (php-mode . "php")
                                     (python-mode . "sc")
                                     (ruby-mode . "rbx")
                                     (text-mode . "txt")
                                     (sql-mode . "sql")
                                     (scheme-mode . "scm")
                                     (smalltalk-mode . "st")
                                     (sh-mode . "sh")
                                     (tcl-mode . "tcl")
                                     (tex-mode . "tex")
                                     (xml-mode . "xml")))

(defun gist-region (begin end &optional private)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                  (file-name-extension file)
                  "txt"))
         (output (generate-new-buffer " *gist*"))
         (login (get-github-user-info))
         (do-private (if private "-F private=1" "")))
    (shell-command-on-region
     begin end
     (format (concat "curl -sS "
                     "%s "
                     "-F 'file_ext[gistfile1]=.%s' "
                     "-F 'file_name[gistfile1]=%s' "
                     "-F 'file_contents[gistfile1]=<-' "
                     "%s "
                     "http://gist.github.com/gists") login ext name do-private)
     output)
    (with-current-buffer output
      (re-search-backward "href=\"\\(.*\\)\"")
      (message "Paste created: %s" (match-string 1))
      (kill-new (match-string 1)))
   (kill-buffer output)))

(defun gist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region (begin end t)))

(defun get-github-user-info (&optional github-user github-key)
  "Asks the user for their github username and api key. If they
don't have them, or wish to paste anonymously, they can do so.
Totally not required."
  (interactive)
  (cond ((and (not (string= github-username ""))
              (not (string= github-api-key "")))
         (format "-F 'login=%s' -F 'token=%s'" github-username github-api-key))
        ((and (> (length github-user) 1)
              (> (length github-key) 1))
         (format "-F 'login=%s' -F 'token=%s'" github-user github-key))
        (t
         (if (or (not github-user)
                 (not github-key))
             (let ((github-user (read-string "GitHub username?: "))
                   (github-key (read-string "GitHub API key?: ")))
               (if (or (string= github-user "")
                       (string= github-key ""))
                   ""
                 (format "-F 'login=%s' -F 'token=%s'" github-user github-key))))
         "")))

(defun gist-buffer ()
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region (point-min) (point-max)))

(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region (point-min) (point-max) t))

(defvar gist-fetch-url "http://gist.github.com/%d.txt"
  "Raw Gist content URL format")

(defun gist-fetch (id)
  "Fetches a Gist and inserts it into a new buffer
If the Gist already exists in a buffer, switches to it"
  (interactive "nGist ID: ")

  (let* ((gist-buffer-name (format "*gist %d*" id)) 
         (gist-buffer (get-buffer gist-buffer-name)))
    (if (bufferp gist-buffer)
      (switch-to-buffer-other-window gist-buffer)
      (progn
        (message "Fetching Gist %d..." id)
        (setq gist-buffer 
              (url-retrieve-synchronously (format gist-fetch-url id)))
        (with-current-buffer gist-buffer 
          (rename-buffer gist-buffer-name t)
          (beginning-of-buffer)
          (search-forward-regexp "\n\n")
          (delete-region (point-min) (point))
          (set-buffer-modified-p nil))
        (switch-to-buffer-other-window gist-buffer)))))

(provide 'gist)
;;; gist.el ends here.