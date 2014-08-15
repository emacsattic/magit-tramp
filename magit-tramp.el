;;; magit-tramp.el --- git method for TRAMP

;; Copyright (C) 2013  Yann Hodique
;; Copyright (C) 1998-2013 Free Software Foundation, Inc.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:
;; Version: 0.1.0
;; Package-Requires: ((magit "1.2.0"))
;; URL: https://github.com/sigma/magit-tramp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'magit)
(require 'tramp)

(defgroup magit-tramp nil
  "git method for TRAMP"
  :link '(url-link "https://github.com/sigma/magit-tramp")
  :prefix "magit-tramp"
  :group 'magit)

;;;###autoload
(defconst magit-tramp-method "git"
  "TRAMP method for browsing git repositories.")

;;;###autoload
(defsubst magit-tramp-file-name-p (filename)
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) magit-tramp-method)))

(defsubst magit-tramp-filerev (user localname)
  (format "%s:%s" user (substring localname 1)))

(defcustom magit-tramp-hosts-alist nil
  "Alist of host -> directory."
  :group 'magit-tramp
  :type '(alist :key-type string :value-type directory))

(defun magit-tramp-handle-file-writable-p (filename)
  nil)

(defun magit-tramp-handle-file-exists-p (filename)
  (with-parsed-tramp-file-name filename target
    (let ((default-directory (magit-tramp-resolve-host target-host)))
      (=
       (magit-git-exit-code "cat-file" "-t"
                            (magit-tramp-filerev target-user target-localname))
       0))))

(defun magit-tramp-canonical-user (user host)
  (if (string-match (rx (or "^" "~")) user)
      (let ((default-directory (magit-tramp-resolve-host host)))
        (magit-rev-parse user))
    user))

(defun magit-tramp-handle-expand-file-name (name &optional dir)
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
	 method (magit-tramp-canonical-user user host) host
	 (tramp-drop-volume-letter
	  (tramp-run-real-handler
	   'expand-file-name (list localname)))
	 hop)))))

(defun magit-tramp-resolve-host (host)
  (let ((elem (assoc host magit-tramp-hosts-alist)))
    (or (and elem
             (cdr elem))
        (and (string-prefix-p "_" host)
             (ignore-errors
               (base64-decode-string (substring host 1)))))))

(defun magit-tramp-gen-host (dirname)
  (let ((elem (rassoc dirname magit-tramp-hosts-alist)))
    (if elem
        (car elem)
        (format "_%s" (base64-encode-string dirname)))))

(defun magit-tramp-file-size (filename)
  (string-to-number
   (with-parsed-tramp-file-name filename target
     (let ((default-directory (magit-tramp-resolve-host target-host)))
       (magit-git-string "cat-file" "-s"
                         (magit-tramp-filerev target-user target-localname))))))

(defun magit-tramp-handle-file-directory-p (filename)
  (with-parsed-tramp-file-name filename target
     (let ((default-directory (magit-tramp-resolve-host target-host)))
       (string= "tree"
                (magit-git-string "cat-file" "-t"
                                  (magit-tramp-filerev target-user
                                                       target-localname))))))

(defun magit-tramp-handle-file-newer-than-file-p (file1 file2)
  ;; bare minimum to make this consistent
  (magit-tramp-handle-file-exists-p file1))

(defun magit-tramp-handle-file-attributes (filename &optional id-format)
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    (with-parsed-tramp-file-name filename nil
      (with-tramp-file-property
	  v localname (format "file-attributes-%s" id-format)
        ;; Reading just the filename entry via "dir localname" is not
        ;; possible, because when filename is a directory, some
        ;; smbclient versions return the content of the directory, and
        ;; other versions don't.  Therefore, the whole content of the
        ;; upper directory is retrieved, and the entry of the filename
        ;; is extracted from.
        (let* ((uid (if (equal id-format 'string) "nobody" -1))
               (gid (if (equal id-format 'string) "nogroup" -1))
               (inode (tramp-get-inode v))
               (device (tramp-get-device v))
               (size (magit-tramp-file-size filename))
               (dir (magit-tramp-handle-file-directory-p filename)))

          ;; Check result.
          (list dir              ;0 file type
                -1	           ;1 link count
                uid	           ;2 uid
                gid	           ;3 gid
                '(0 0)	   ;4 atime
                '(0 0)           ;5 mtime
                '(0 0)	   ;6 ctime
                size                ;7 size
                (if dir "dr-xr-xr-x" "-r--r--r--")     ;8 mode
                nil	           ;9 gid weird
                inode	           ;10 inode number
                device           ;11 file system number
                ))))))

(defun magit-tramp-handle-insert-file-contents (filename &optional visit
                                                           beg end replace)
  (if replace
      (list filename 0)
      (when visit
        (setq buffer-file-name filename))
      (insert (substring
               (with-parsed-tramp-file-name filename target
                 (string-as-multibyte
                  (let ((default-directory (magit-tramp-resolve-host
                                            target-host)))
                    (magit-git-insert
                     (list "cat-file" "-p"
                           (magit-tramp-filerev target-user
                                                target-localname))))))
               (or beg 0) end))
      (list filename (magit-tramp-file-size filename))))

(defun magit-tramp-handle-file-name-all-completions (file directory)
  (with-parsed-tramp-file-name directory target
    (let ((default-directory (magit-tramp-resolve-host target-host)))
      (delete nil
              (mapcar (lambda (name)
                        (and (string-prefix-p file name)
                             name))
                      (magit-git-lines
                       "ls-tree" "--name-only"
                       (magit-tramp-filerev target-user target-localname)))))))

(defun magit-tramp-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format)
  (mapcar (lambda (filename)
            (cons filename (magit-tramp-handle-file-attributes
                            (expand-file-name filename directory)
                            id-format)))
          (tramp-handle-directory-files directory)))

(defun magit-tramp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  (setq filename (expand-file-name filename))
  (if full-directory-p
      ;; Called from `dired-add-entry'.
      (setq filename (file-name-as-directory filename))
    (setq filename (directory-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    (save-match-data
      (let ((base (file-name-nondirectory filename))
	    (entries (magit-tramp-handle-directory-files-and-attributes
                      (file-name-directory filename)
                      nil nil nil 'string)))

        (unless (string= localname "/")
          (push '(".." t -1 "nobody" "nogroup" (0 0) (0 0) (0 0)
                  0 "dr-xr-xr-x" nil
                  (tramp-get-inode v)
                  (tramp-get-device v))
                entries))

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
		  (format
		   "%s\n" (nth 0 x)))
		 (put-text-property start (1- (point)) 'dired-filename t))
	       (forward-line)
	       (beginning-of-line))))
	 entries)))))

(defun magit-tramp-handle-file-local-copy (filename)
  (with-parsed-tramp-file-name filename nil
    (unless (magit-tramp-handle-file-exists-p filename)
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

(defconst magit-tramp-file-name-handler-alist
  '((load . tramp-handle-load)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-truename . identity)
    (file-exists-p . magit-tramp-handle-file-exists-p)
    (file-directory-p . magit-tramp-handle-file-directory-p)
    ;; executable iff directory
    (file-executable-p . magit-tramp-handle-file-directory-p)
    ;; readable iff exists
    (file-readable-p . magit-tramp-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    ;; TODO: support symlinks
    (file-symlink-p . ignore)
    (file-writable-p . magit-tramp-handle-file-writable-p)
    (file-ownership-preserved-p . ignore)
    (file-newer-than-file-p . magit-tramp-handle-file-newer-than-file-p)
    (file-attributes . magit-tramp-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . magit-tramp-handle-directory-files-and-attributes)
    (file-name-all-completions . magit-tramp-handle-file-name-all-completions)
    (file-name-completion . tramp-handle-file-name-completion)
    (add-name-to-file . ignore)
    (copy-file . ignore)
    (copy-directory . ignore)
    (rename-file . ignore)
    (set-file-modes . ignore)
    (set-file-times . ignore)
    (make-directory . ignore)
    (delete-directory . ignore)
    (delete-file . ignore)
    (directory-file-name . tramp-handle-directory-file-name)
    ;; `executable-find' is not official yet.
    (executable-find . ignore)
    (start-file-process . ignore)
    (process-file . ignore)
    (shell-command . ignore)
    (insert-directory . magit-tramp-handle-insert-directory)
    (expand-file-name . magit-tramp-handle-expand-file-name)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (file-local-copy . magit-tramp-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (insert-file-contents . magit-tramp-handle-insert-file-contents)
    ;; TODO: do we need more logic here ?
    (insert-file-contents-literally
     . magit-tramp-handle-insert-file-contents)
    (write-region . ignore)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
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
(defun magit-tramp-file-name-handler (operation &rest args)
  (when (and tramp-locked (not tramp-locker))
    (setq tramp-locked nil)
    (signal 'file-error (list "Forbidden reentrant call of Tramp")))
  (let ((tl tramp-locked))
    (unwind-protect
	(progn
	  (setq tramp-locked t)
	  (let ((tramp-locker t))
	    (save-match-data
	      (let ((fn (assoc operation magit-tramp-file-name-handler-alist)))
		(if fn
		    (apply (cdr fn) args)
		  (tramp-run-real-handler operation args))))))
      (setq tramp-locked tl))))

;;;###autoload
(eval-after-load 'tramp
  '(progn
    (add-to-list 'tramp-methods (cons magit-tramp-method nil))
    (add-to-list 'tramp-foreign-file-name-handler-alist
     '(magit-tramp-file-name-p . magit-tramp-file-name-handler))))

(provide 'magit-tramp)
;;; magit-tramp.el ends here
