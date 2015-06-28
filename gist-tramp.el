;;; gist-tramp.el --- gist method for TRAMP

;; Copyright (C) 2013  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'tramp)
(require 'tramp-sh)

(require 'gh-gist)
(require 'gh-profile)
(require 'gh-users)

;;;###autoload
(defconst gist-tramp-method "gist"
  "TRAMP method for browsing gists.")

(defgroup gist-tramp nil
  "Gist TRAMP method."
  :group 'tramp)

(defcustom gist-tramp-following-alist '(("github"))
  "List of people who have gists of interests, per gh profile."
  :type '(alist :key-type string
                :value-type (repeat string))
  :group 'gist-tramp)

(defconst gist-tramp-default-user
  (list 'integer (nth 2 (file-attributes "~" 'integer))
	'string  (nth 2 (file-attributes "~" 'string)))
  "Default user.")

(defconst gist-tramp-default-group
  (list 'integer (nth 2 (file-attributes "~" 'integer))
	'string  (nth 2 (file-attributes "~" 'string)))
  "Default group.")

;;;###autoload
(defsubst gist-tramp-file-name-p (filename)
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) gist-tramp-method)))

(defun gist-tramp-get-gh-api (host &optional cls)
  (let* ((gh-profile-current-profile host))
    (funcall (or cls 'gh-gist-api)
             "api" :sync t :cache t :num-retries 1)))

(defun gist-tramp-gh-current-user (host)
  (oref
   (oref (gh-users-get
          (gist-tramp-get-gh-api host 'gh-users-api))
         :data)
   :login))

(defun gist-tramp-dissect-file-name (localname)
  (let ((l (split-string localname "/" t)))
    (cond ((= (length l) 3)
           l)
          ((> (length l) 0)
           (if (string-match "^[0-9]+$" (nth 0 l))
               (cons nil l)
             l))
          (t l))))

(defsubst gist-tramp-filename-owner (v)
  (car v))

(defsubst gist-tramp-filename-gist (v)
  (cadr v))

(defsubst gist-tramp-filename-file (v)
  (car (cddr v)))

(defmacro with-gist-parsed-tramp-file-name (filename var &rest body)
  (let* ((gist-var (intern (format "%s--gist" (symbol-name (or var 'v)))))
         (bindings
          (mapcar (lambda (elem)
                    `(,(if var (intern (format "%s-%s" var elem)) elem)
                      (,(intern (format "gist-tramp-filename-%s" elem))
                       ,gist-var)))
                 '(owner gist file))))
    `(with-parsed-tramp-file-name ,filename ,var
       (let* ((,gist-var (gist-tramp-dissect-file-name
                          ,(intern (or (and var (format "%s-localname" var))
                                       "localname"))))
              ,@bindings) 
         ,@body))))

(put 'with-gist-parsed-tramp-file-name 'lisp-indent-function 2)
(put 'with-gist-parsed-tramp-file-name 'edebug-form-spec '(form symbolp body))
(tramp-compat-font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-gist-parsed-tramp-file-name\\>"))

(defun gist-tramp-handle-file-exists-p (filename)
  (condition-case nil
      (with-gist-parsed-tramp-file-name filename target
        (let ((api (gist-tramp-get-gh-api target-host)))
          (cond (target-gist
                 (let* ((gist (oref (gh-gist-get api target-gist) :data))
                        (files (oref gist :files)))
                   (and
                    (or (not target-owner)
                        (string= target-owner (oref (oref gist :user) :login)))
                    (or (not target-file)
                        (loop for f in files
                           if (string= target-file (oref f :filename))
                           return f)))))
                (target-owner
                 (oref (gh-gist-list api target-owner) :data))
                (t t))))
    (error nil)))

(defun gist-tramp-handle-file-directory-p (filename)
  (condition-case nil
      (with-gist-parsed-tramp-file-name filename target
        (and (not target-file)
             (gist-tramp-handle-file-exists-p filename)))))

(defun gist-tramp-handle-file-writable-p (filename)
  (condition-case nil
      (with-gist-parsed-tramp-file-name filename target
        (string= target-owner
                 (gist-tramp-gh-current-user target-host)))))

(defun gist-tramp-handle-file-newer-than-file-p (file1 file2)
  ;; bare minimum to make this consistent
  (and file1
       (gist-tramp-handle-file-exists-p file1)))

(defun gist-tramp-file-size (filename)
  (or
   (and (not (gist-tramp-handle-file-directory-p filename))
        (let ((f (gist-tramp-handle-file-exists-p filename)))
          (oref f :size)))
   1))

(defun gist-tramp-handle-file-attributes (filename &optional id-format)
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    (with-gist-parsed-tramp-file-name filename nil
      (with-tramp-file-property
	  v localname (format "file-attributes-%s" id-format)
        ;; Reading just the filename entry via "dir localname" is not
        ;; possible, because when filename is a directory, some
        ;; smbclient versions return the content of the directory, and
        ;; other versions don't.  Therefore, the whole content of the
        ;; upper directory is retrieved, and the entry of the filename
        ;; is extracted from.
        (let* ((myself (string= owner
                                (gist-tramp-gh-current-user host)))
               (uid (if myself
                        (plist-get gist-tramp-default-user id-format)
                      (if (equal id-format 'string) "nobody" -1)))
               (gid (if myself
                        (plist-get gist-tramp-default-group id-format)
                      (if (equal id-format 'string) "nogroup" -1)))
               (inode (tramp-get-inode v))
               (device (tramp-get-device v))
               (size (gist-tramp-file-size filename))
               (dir (gist-tramp-handle-file-directory-p filename)))

          ;; Check result.
          (list dir              ;0 file type
                -1	           ;1 link count
                uid	           ;2 uid
                gid	           ;3 gid
                '(0 0)	   ;4 atime
                '(0 0)           ;5 mtime
                '(0 0)	   ;6 ctime
                size                ;7 size
                (if dir
                    (if myself
                        "drwxr-xr-x"
                      "dr-xr-xr-x")
                  (if (gist-tramp-handle-file-writable-p filename)
                      "-rw-rw-rw-"
                    "-r--r--r--"))  ;8 mode
                nil	           ;9 gid weird
                inode	           ;10 inode number
                device           ;11 file system number
                ))))))

(defun gist-tramp-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format)
  (mapcar (lambda (filename)
            (cons filename (gist-tramp-handle-file-attributes
                            (expand-file-name filename directory)
                            id-format)))
          (tramp-handle-directory-files directory)))

(defun gist-tramp-handle-file-name-all-completions (file directory)
  (with-gist-parsed-tramp-file-name directory target
    (and (gist-tramp-handle-file-exists-p directory)
         (let* ((api (gist-tramp-get-gh-api target-host))
                (items
                 (cond (target-gist
                        (mapcar (lambda (f) (oref f :filename))
                                (oref
                                 (oref (gh-gist-get api target-gist) :data)
                                 :files)))
                       (target-owner
                        (mapcar (lambda (g) (oref g :id))
                                (oref (gh-gist-list api target-owner) :data)))
                       (t
                        (cons (gist-tramp-gh-current-user target-host)
                              (cdr (assoc target-host
                                          gist-tramp-following-alist)))))))
           (delete nil
              (mapcar (lambda (name)
                        (and (string-prefix-p file name)
                             name))
                      items))))))

(defun gist-tramp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  (setq filename (expand-file-name filename))
  (if full-directory-p
      ;; Called from `dired-add-entry'.
      (setq filename (file-name-as-directory filename))
    (setq filename (directory-file-name filename)))
  (with-gist-parsed-tramp-file-name filename nil
    (save-match-data
      (let ((base (file-name-nondirectory filename))
	    (entries (gist-tramp-handle-directory-files-and-attributes
                      (file-name-directory filename)
                      nil nil nil 'string)))

        (unless (string= localname "/")
          (let* ((own (string= owner
                           (gist-tramp-gh-current-user host)))
                 (owner
                  (if own
                      (plist-get gist-tramp-default-user 'string)
                    "nobody"))
                 (group
                  (if own
                      (plist-get gist-tramp-default-group 'string)
                    "nogroup")))
            (push `(".." t -1 ,owner ,group (0 0) (0 0) (0 0)
                         0 "dr-xr-xr-x" nil
                         ,(tramp-get-inode v)
                         ,(tramp-get-device v))
                  entries)))

	(when wildcard
	  ;; (string-match "\\." base)
	  ;; (setq base (replace-match "\\\\." nil nil base))
	  ;; (string-match "\\*" base)
	  ;; (setq base (replace-match ".*" nil nil base))
	  ;; (string-match "\\?" base)
	  ;; (setq base (replace-match ".?" nil nil base))
          )

	;; Filter entries.
	(setq entries
	      (delq
	       nil
	       (if (or wildcard (zerop (length base)))
		   ;; Check for matching entries.
		   (mapcar
		    (lambda (x)
		      (when (string-match
			     (format "^%s" base) (nth 0 x))
			x))
		    entries)
		 ;; We just need the only and only entry FILENAME.
		 (list (assoc base entries)))))

	;; Sort entries.
	(setq entries
	      (sort
	       entries
	       (lambda (x y)
		 (if (string-match "t" switches)
		     ;; Sort by date.
		     (tramp-time-less-p (nth 3 y) (nth 3 x))
		   ;; Sort by name.
		   (string-lessp (nth 0 x) (nth 0 y))))))

	;; Handle "-F" switch.
	(when (string-match "F" switches)
	  (mapc
	   (lambda (x)
	     (when (not (zerop (length (car x))))
	       (cond
		((char-equal ?d (string-to-char (nth 1 x)))
		 (setcar x (concat (car x) "/")))
		((char-equal ?x (string-to-char (nth 1 x)))
		 (setcar x (concat (car x) "*"))))))
	   entries))

	;; Print entries.
        (mapc
	 (lambda (x)
	   (when (not (zerop (length (nth 0 x))))
	     (let ((attr (ignore-errors
                           (cdr x))))
               (insert
		(format
		 "%10s %3d %-8s %-8s %8s %s "
		 (or (nth 8 attr)) ; mode
		 (or (nth 1 attr)) ; inode
		 (or (nth 2 attr)) ; uid
		 (or (nth 3 attr)) ; gid
		 (or (nth 7 attr)) ; size
		 (format-time-string
		  (if (tramp-time-less-p
		       (tramp-time-subtract (current-time) (nth 5 attr))
		       tramp-half-a-year)
		      "%b %e %R"
		    "%b %e  %Y")
		  (nth 5 attr)))) ; date
	       ;; We mark the file name.  The inserted name could be
	       ;; from somewhere else, so we use the relative file
	       ;; name of `default-directory'.
	       (let ((start (point)))
		 (insert
		  (format "%s\n" (nth 0 x)))
		 (put-text-property start (1- (point)) 'dired-filename t))
	       (forward-line)
	       (beginning-of-line))))
	 entries)))))

(defun gist-tramp-handle-expand-file-name (name &optional dir)
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-connectable-p name))
      (tramp-run-real-handler 'expand-file-name (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (tramp-run-real-handler 'file-name-absolute-p (list localname))
	(setq localname (concat "~/" localname)))
      (while (string-match "//" localname)
	(setq localname (replace-match "/" t t localname)))
      (let ((directory-sep-char ?/)
	    (default-directory (tramp-compat-temporary-file-directory)))
	(tramp-make-tramp-file-name
	 method user host
	 (tramp-drop-volume-letter
	  (tramp-run-real-handler
	   'expand-file-name (list localname)))
	 hop)))))

(defun gist-tramp-handle-insert-file-contents (filename &optional visit
                                                          beg end replace)
  (if replace
      (list filename 0)
      (when visit
        (setq buffer-file-name filename))
      (ignore-errors
        (insert (substring
                 (with-gist-parsed-tramp-file-name filename target
                   (string-as-multibyte
                    (let* ((api (gist-tramp-get-gh-api target-host))
                           (gist (oref (gh-gist-get api target-gist) :data)))
                      (loop for f in (oref gist :files)
                         if (string= target-file (oref f :filename))
                         return (oref f :content)))))
                 (or beg 0) end)))
      (list filename (or (ignore-errors (gist-tramp-file-size filename))
                         0))))

(defun gist-tramp-handle-file-local-copy (filename)
  (with-parsed-tramp-file-name filename nil
    (unless (gist-tramp-handle-file-exists-p filename)
      (tramp-error
       v 'file-error
       "Cannot make local copy of non-existing file `%s'" filename))

    (let* ((tmpfile (tramp-compat-make-temp-file filename)))
      (condition-case err
          (save-excursion
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents filename)

              ;; Unset `file-name-handler-alist'.  Otherwise,
              ;; epa-file gets confused.
              (let (file-name-handler-alist
                    (coding-system-for-write 'binary))
                (write-region (point-min) (point-max) tmpfile)))

            ;; Set proper permissions.
            (set-file-modes tmpfile (tramp-default-file-modes filename))
            ;; Set local user ownership.
            (tramp-set-file-uid-gid tmpfile))

	;; Error handling.
	((error quit)
	 (delete-file tmpfile)
	 (signal (car err) (cdr err))))

      (run-hooks 'tramp-handle-file-local-copy-hook)
      tmpfile)))

(defun gist-tramp-handle-write-region (start end filename
                                       &optional append visit
                                         lockname mustbenew)
  (with-gist-parsed-tramp-file-name filename target
    (let* ((api (gist-tramp-get-gh-api target-host))
           (gist (oref (gh-gist-get api target-gist) :data))
           (files (oref gist :files))
           (content ""))
      (when append
        (let ((file (loop for f in files
                       if (string= target-file (oref f :filename))
                       return f)))
          (setq content (oref file :content))))
      (setq content
            (concat content
                    (cond ((null start)
                           (buffer-string))
                          ((stringp start)
                           start)
                          (t
                           (buffer-substring start end)))))
      (setq files
            (cons (make-instance 'gh-gist-gist-file
                                 :filename target-file
                                 :content content)
                  (loop for f in files
                     if (not (string= target-file (oref f :filename)))
                     collect f)))
      (gh-gist-edit api (clone gist :files files)))))

(defun gist-tramp-handle-delete-file (filename &optional trash)
  (with-gist-parsed-tramp-file-name filename target
    (let* ((api (gist-tramp-get-gh-api target-host))
           (gist (oref (gh-gist-get api target-gist) :data))
           (files (oref gist :files)))
      (setq files
            (cons (make-instance 'gh-gist-gist-file
                                 :filename target-file
                                 :content nil)
                  (loop for f in files
                     if (not (string= target-file (oref f :filename)))
                     collect f)))
      (gh-gist-edit api (clone gist :files files)))))

(defconst gist-tramp-file-name-handler-alist
  '((load . tramp-handle-load)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-truename . identity)
    (file-exists-p . gist-tramp-handle-file-exists-p)
    (file-directory-p . gist-tramp-handle-file-directory-p)
    ;; executable iff directory
    (file-executable-p . gist-tramp-handle-file-directory-p)
    ;; readable iff exists
    (file-readable-p . gist-tramp-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    ;; TODO: support symlinks
    (file-symlink-p . ignore)
    (file-writable-p . gist-tramp-handle-file-writable-p)
    (file-ownership-preserved-p . ignore)
    (file-newer-than-file-p . gist-tramp-handle-file-newer-than-file-p)
    (file-attributes . gist-tramp-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . gist-tramp-handle-directory-files-and-attributes)
    (file-name-all-completions . gist-tramp-handle-file-name-all-completions)
    (file-name-completion . tramp-handle-file-name-completion)
    (add-name-to-file . ignore)
    (copy-file . ignore)
    (copy-directory . ignore)
    (rename-file . ignore)
    (set-file-modes . ignore)
    (set-file-times . ignore)
    (make-directory . ignore)
    (delete-directory . ignore)
    (delete-file . gist-tramp-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    ;; `executable-find' is not official yet.
    (executable-find . ignore)
    (start-file-process . ignore)
    (process-file . ignore)
    (shell-command . ignore)
    (insert-directory . gist-tramp-handle-insert-directory)
    (expand-file-name . gist-tramp-handle-expand-file-name)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (file-local-copy . gist-tramp-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (insert-file-contents . gist-tramp-handle-insert-file-contents)
    ;; TODO: do we need more logic here ?
    (insert-file-contents-literally
     . gist-tramp-handle-insert-file-contents)
    (write-region . gist-tramp-handle-write-region)
    (find-backup-file-name . ignore)
    ;; gists are versioned, no need to auto-save
    (make-auto-save-file-name . ignore)
    (unhandled-file-name-directory . ignore)
    (dired-compress-file . ignore)
    (dired-recursive-delete-directory . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (set-visited-file-modtime . ignore)
    (verify-visited-file-modtime . (lambda (&rest args) t))
    (file-selinux-context . (lambda (&rest args) '(nil nil nil nil)))
    (set-file-selinux-context . ignore)
    (vc-registered . ignore)))

;;;###autoload
(defun gist-tramp-file-name-handler (operation &rest args)
  (when (and tramp-locked (not tramp-locker))
    (setq tramp-locked nil)
    (signal 'file-error (list "Forbidden reentrant call of Tramp")))
  (let ((tl tramp-locked))
    (unwind-protect
	(progn
	  (setq tramp-locked t)
	  (let ((tramp-locker t))
	    (save-match-data
	      (let ((fn (assoc operation gist-tramp-file-name-handler-alist)))
		(if fn
		    (apply (cdr fn) args)
		  (tramp-run-real-handler operation args))))))
      (setq tramp-locked tl))))

;;;###autoload
(eval-after-load 'tramp
  '(progn
    (add-to-list 'tramp-methods (cons gist-tramp-method nil))
    (add-to-list 'tramp-foreign-file-name-handler-alist
     '(gist-tramp-file-name-p . gist-tramp-file-name-handler))))

(provide 'gist-tramp)
;;; gist-tramp.el ends here
