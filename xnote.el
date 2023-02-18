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

(require xnote-db)
(require xnote-util)

(xnote-db-query [:select * :from note])

(defun xnote-current-seconds ()
  "Return seconds of current time."
  (time-convert (current-time) 'integer))

(defun xnote-add (status n)
  (let ((id (org-id-uuid))
        (time (xnote-current-seconds)))
    (xnote-db-query `[:insert :into note
                              :values ([,id ,status ,(concat "笔记" n "的内容")
                                            ,(concat "笔记" n "的概要") nil nil nil ,time])])))

(defun xnote-query (n)
  (caar (xnote-db-query `[:select id :from note :where (= content ,(concat "笔记" n "的内容"))])))

(defun xnote-update (n id)
  (xnote-db-query `[:update note :set (= child ,id)
                            :where (= content ,(concat "笔记" n "的内容"))]))

(defun xnote-evolve (n1 n2)
  "evlove from n1 to n2"
  (xnote-update n1 (xnote-query n2)))

(defun xnote-query-child (n)
  (caar (xnote-db-query `[:select child :from note :where (= content ,(concat "笔记" n "的内容"))])))

(xnote-evolve "2" "a")
(xnote-evolve "a" "b")
(xnote-evolve "b" "c")

(xnote-query "2")
(xnote-query "a")
(xnote-query "b")
(xnote-query "c")

(xnote-query-child "2")
(xnote-query-child "a")
(xnote-query-child "b")
(xnote-query-child "c")
;; 2: 842cc0f7-3214-48d5-bf91-4e54eb38521b
;; a: 32791a69-59f1-404d-9618-37d7d185f236
;; b: 2424d01a-1350-4028-a087-42e981567f8b
;; c: f46ecd75-d889-42de-9731-3ac71c3656a0

;; (xnote-add "1")
;; (xnote-add "2")
;; (xnote-add "3")
;; (xnote-add "4")
;; (xnote-add "5")

(xnote-add 1 "a")
(xnote-add 1 "b")
(xnote-add 1 "c")
(xnote-add 1 "d")
(xnote-add 1 "e")
(xnote-add 1 "f")

(defun xnote-time-list (date)
  "Return a time list of original notes create on a specific DATE."
  (let* ((seconds (xnote-date-to-seconds date))
         (next-date-seconds (+ seconds (* 24 60 60))))
    (mapcar #'car (xnote-db-query `[:select time :from note
                                            :where (and (= status 0)
                                                        (>= time ,seconds)
                                                        (< time ,next-date-seconds))]))))
;; (xnote-time-list "2023-02-18")

(defun xnote-query-by-time (seconds)
  (car (xnote-db-query `[:select id :from note :where (= seconds ,seconds)])))

(defun xnote-tree-list (id tree)
  (let ((child-id (caar (xnote-db-query `[:select child :from note
                                                  :where (= id ,id)]))))
    (when child-id
      (push child-id tree)
      ;; (message "id: %s" child-id)
      (xnote-tree-list child-id tree))))

(let (tree)
  (xnote-tree-list (xnote-query "2") tree)
  (message "%s" tree))
