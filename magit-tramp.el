;;; magit-tramp.el --- git method for TRAMP

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

(require 'magit)
(require 'tramp)

;;;###autoload
(defconst magit-tramp-method "git"
  "TRAMP method for browsing git repositories.")

;;;###autoload
(defsubst magit-tramp-file-name-p (filename)
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) magit-tramp-method)))

(defvar magit-tramp-hosts-alist nil
  "Alist of host -> directory")

(defun magit-tramp-handle-file-writable-p (filename)
  nil)

(defun magit-tramp-handle-file-exists-p (filename)
  (with-parsed-tramp-file-name filename target
    (let ((default-directory (magit-tramp-resolve-host target-host)))
      (=
       (magit-git-exit-code "cat-file" "-t"
                            (format "%s:%s" target-user
                                    target-localname))
       0))))

(defun magit-tramp-handle-expand-file-name
    (filename &optional default-directory)
  (if (or (not default-directory) (string-prefix-p "/" filename))
      filename
      (concat default-directory filename)))

(defun magit-tramp-handle-file-remote-p (filename
                                         &optional identification connected)
  t)

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
                         (format "%s:%s" target-user
                                 target-localname))))))

(defun magit-tramp-handle-file-directory-p (filename)
  (with-parsed-tramp-file-name filename target
     (let ((default-directory (magit-tramp-resolve-host target-host)))
       (string= "tree"
                (magit-git-string "cat-file" "-t"
                                  (format "%s:%s" target-user
                                          target-localname))))))

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
                  (let ((default-directory (magit-tramp-resolve-host target-host)))
                    (magit-git-output (list "cat-file" "-p"
                                            (format "%s:%s" target-user
                                                    target-localname))))))
               (or beg 0) end))
      (list filename (magit-tramp-file-size filename))))

(defun magit-tramp-handle-insert-directory
    (filename switches &optional wildcard full-directory-p)
  nil)

(defconst magit-tramp-file-name-handler-alist
  '(
    (load . tramp-handle-load)
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
    ;; (file-regular-p . tramp-handle-file-regular-p)
    ;; (file-symlink-p . tramp-handle-file-symlink-p)
    (file-writable-p . magit-tramp-handle-file-writable-p)
    ;; (file-ownership-preserved-p . tramp-sh-handle-file-ownership-preserved-p)
    ;; (file-newer-than-file-p . tramp-sh-handle-file-newer-than-file-p)
    (file-attributes . magit-tramp-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (directory-files . tramp-handle-directory-files)
    ;; (directory-files-and-attributes
    ;;  . tramp-sh-handle-directory-files-and-attributes)
    ;; (file-name-all-completions . tramp-sh-handle-file-name-all-completions)
    (file-name-completion . tramp-handle-file-name-completion)
    ;; (add-name-to-file . tramp-sh-handle-add-name-to-file)
    ;; (copy-file . tramp-sh-handle-copy-file)
    ;; (copy-directory . tramp-sh-handle-copy-directory)
    ;; (rename-file . tramp-sh-handle-rename-file)
    ;; (set-file-modes . tramp-sh-handle-set-file-modes)
    ;; (set-file-times . tramp-sh-handle-set-file-times)
    ;; (make-directory . tramp-sh-handle-make-directory)
    ;; (delete-directory . tramp-sh-handle-delete-directory)
    ;; (delete-file . tramp-sh-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    ;; ;; `executable-find' is not official yet.
    ;; (executable-find . tramp-sh-handle-executable-find)
    ;; (start-file-process . tramp-sh-handle-start-file-process)
    ;; (process-file . tramp-sh-handle-process-file)
    ;; (shell-command . tramp-handle-shell-command)
    (insert-directory . magit-tramp-handle-insert-directory)
    (expand-file-name . magit-tramp-handle-expand-file-name)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    ;; (file-local-copy . tramp-sh-handle-file-local-copy)
    (file-remote-p . magit-tramp-handle-file-remote-p)
    (insert-file-contents . magit-tramp-handle-insert-file-contents)
    ;; (insert-file-contents-literally
    ;;  . tramp-sh-handle-insert-file-contents-literally)
    ;; (write-region . tramp-sh-handle-write-region)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; (make-auto-save-file-name . tramp-sh-handle-make-auto-save-file-name)
    (unhandled-file-name-directory . ignore)
    ;; (dired-compress-file . tramp-sh-handle-dired-compress-file)
    ;; (dired-recursive-delete-directory
    ;;  . tramp-sh-handle-dired-recursive-delete-directory)
    (dired-uncache . tramp-handle-dired-uncache)
    ;; (set-visited-file-modtime . tramp-sh-handle-set-visited-file-modtime)
    ;; (verify-visited-file-modtime . tramp-sh-handle-verify-visited-file-modtime)
    ;; (file-selinux-context . tramp-sh-handle-file-selinux-context)
    ;; (set-file-selinux-context . tramp-sh-handle-set-file-selinux-context)
    ;; (vc-registered . tramp-sh-handle-vc-registered)
    ))

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
