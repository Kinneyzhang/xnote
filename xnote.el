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

(require 'xnote-util)
(require 'xnote-face)
(require 'xnote-db)

(defvar xnote-temp-evoltree-list nil
  "A temp variable to store evoltree list.")

(defun xnote-timeline-data (date)
  "Return original notes data create on a specific DATE."
  (let* ((seconds (xnote-date-to-seconds date))
         (next-date-seconds (+ seconds (* 24 60 60))))
    (xnote-wrap-plist
     (xnote-db-query
      `[:select [time id] :from note
                :where (and (= status 0)
                            (>= time ,seconds)
                            (< time ,next-date-seconds))])
     :time :id)))

(defun xnote--evoltree-data (id)
  (let ((child-id (caar (xnote-db-query `[:select child :from note :where (= id ,id)])))
        child-resume)
    (when child-id
      (setq child-resume (caar (xnote-db-query `[:select resume :from note :where (= id ,child-id)])))
      (setq xnote-temp-evoltree-list (append xnote-temp-evoltree-list (list (list child-id child-resume))))
      (xnote--evoltree-data child-id))))

(defun xnote-evoltree-data (id)
  "Return a plist data of note in evoltree, evolve from note with ID."
  (setq xnote-temp-evoltree-list nil)
  (xnote--evoltree-data id)
  (xnote-wrap-plist xnote-temp-evoltree-list :id :resume))

;; (xnote-evoltree-data (xnote-query "2"))

(defun xnote-content-detail (id)
  (caar (xnote-db-query `[:select content :from note :where (= id ,id)])))
;; (xnote-content-detail (xnote-query "m"))

;;; display

(defvar xnote-timeline-buffer "*xnote timeline*")

(defvar xnote-evoltree-buffer "*xnote evoltree*")

(defvar xnote-content-buffer "*xnote content*")

(defvar xnote-return-window-confs nil)

(defvar xnote-timeline-window-height 30)
;; (defvar xnote-evoltree-window-width 15)
;; (defvar xnote-evoltree-window-width 15)

;; (defun xnote-draw-windows ()
;;   (interactive)
;;   (setq xnote-return-window-confs (current-window-configuration))
;;   (delete-other-windows)
;;   (split-window-vertically xnote-timeline-window-height)
;;   (other-window 1)
;;   (split-window-horizontally)
;;   (other-window -1)
;;   (xnote-timeline-show)
;;   (other-window 1)
;;   (xnote-evoltree-show)
;;   (other-window 1)
;;   (xnote-content-show))

(defun xnote-draw-windows ()
  (interactive)
  (setq xnote-return-window-confs (current-window-configuration))
  (delete-other-windows)
  (split-window-horizontally xnote-timeline-window-height)
  (other-window 1)
  (split-window-horizontally)
  (other-window -1)
  (xnote-timeline-show)
  (other-window 1)
  (xnote-evoltree-show)
  (other-window 1)
  (xnote-content-show))

(defun xnote-format-timepoint (seconds)
  (format-time-string "%H:%M:%S" seconds))

(defvar xnote-current-id nil)

(defun xnote-timeline-show ()
  (interactive)
  (with-current-buffer (get-buffer-create xnote-timeline-buffer)
    (erase-buffer)
    (let* ((date "2023-02-19") ;; (xnote-current-date)
           (data (xnote-timeline-data date)))
      (setq xnote-current-id (plist-get (car (last data)) :id))
      (insert (propertize date
                          'face 'xnote-timeline-date-face
                          'line-prefix "  "
                          'wrap-prefix "  ")
              "\n\n")
      (dolist (info data)
        (insert (propertize (xnote-format-timepoint (plist-get info :time))
                            'face 'region
                            'id (plist-get info :id)
                            'line-prefix "  "
                            'wrap-prefix "  ")
                "\n\n"))))
  (switch-to-buffer xnote-timeline-buffer))

;; (xnote-timeline-data "2023-02-19")

(defun xnote-evoltree-show ()
  (with-current-buffer (get-buffer-create xnote-evoltree-buffer)
    (erase-buffer)
    (let ((data (xnote-evoltree-data xnote-current-id)))
      (dolist (info data)
        (insert (plist-get info :resume) "\n\n"))))
  (switch-to-buffer xnote-evoltree-buffer))

(defun xnote-content-show ()
  (with-current-buffer (get-buffer-create xnote-content-buffer)
    (erase-buffer)
    (insert "content"))
  (switch-to-buffer xnote-content-buffer))

;; (set-window-configuration xnote-return-window-confs)

(defun xnote-show ()
  )

(provide 'xnote)
