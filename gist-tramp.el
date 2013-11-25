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

(require 'gh-gist)
(require 'gh-profile)

;;;###autoload
(defconst gist-tramp-method "gist"
  "TRAMP method for browsing gists.")

;;;###autoload
(defsubst gist-tramp-file-name-p (filename)
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) gist-tramp-method)))

(defun gist-tramp-get-gh-api (host)
  (let* ((gh-profile-current-profile host))
    (gh-gist-api "api" :sync t :cache t :num-retries 1)))

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
                        (and
                         (member target-file
                                 (mapcar
                                  (lambda (f) (oref f :filename)) files))
                         t)))))
                (target-owner
                 (and (oref (gh-gist-list api target-owner) :data) t))
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
        (and (gist-tramp-handle-file-exists-p filename)
             ;; TODO: compare owner to current user
             ))))

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
