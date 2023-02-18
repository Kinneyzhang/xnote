;;; xnote.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: zettelkasten convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/xnote
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;;; Code:

;;;; Requires

(require 'emacsql)
(require 'emacsql-sqlite)

(defvar xnote-db-file
  (expand-file-name "xnote.db" (concat user-emacs-directory "xnote")))

(defvar xnote-db--conn (make-hash-table :test #'equal)
  "Database connection to xnote-db.")

(defconst xnote-db--table-schemata
  '((note
     [(id :primary-key)
      (status :not-null)
      (content :not-null)
      resume child refs tag time])))

(defun xnote-db--get-conn ()
  "Return the xnote database connection with key PATH."
  (gethash xnote-db-file xnote-db--conn))

(defun xnote-db--init (db)
  "Initialize database DB with `xnote-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) xnote-db--table-schemata)
      (emacsql db `[:create-table ,table ,schema]))))

(defun xnote-db ()
  "Entrypoint to xnote sqlite database."
  (unless (and (xnote-db--get-conn)
               (emacsql-live-p (xnote-db--get-conn)))
    (let ((init-db (not (file-exists-p xnote-db-file))))
      (make-directory (file-name-directory xnote-db-file) t)
      (let ((conn (emacsql-sqlite xnote-db-file)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash xnote-db-file conn xnote-db--conn)
        (when init-db
          (xnote-db--init conn)))))
  (xnote-db--get-conn))

(defun xnote-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for current xnote db."
  (unless db
    (setq db (xnote-db--get-conn)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun xnote-db-query (sql &rest args)
  "Return SQL query on xnote database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (xnote-db) (apply #'format sql args))
    (apply #'emacsql (xnote-db) sql args)))

(defun xnote-db-clear ()
  "Clear all data in xnote database."
  (interactive)
  (when (file-exists-p xnote-db-file)
    (dolist (table (mapcar #'car xnote-db--table-schemata))
      (xnote-db-query `[:delete :from ,table]))))

(defun xnote-db-drop ()
  "Drop the whole xnote database."
  (interactive)
  (xnote-db--close)
  (delete-file xnote-db-file))

(provide 'xnote-db)
