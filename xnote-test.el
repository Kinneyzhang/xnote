(require 'xnote)

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


(xnote-query "2")
(xnote-query "a")
(xnote-query "b")
(xnote-query "c")

(xnote-query "9")
(xnote-query "l")

(xnote-query-child "9")

(xnote-query-child "2")
(xnote-query-child "a")
(xnote-query-child "b")
(xnote-query-child "c")
;; 2: 842cc0f7-3214-48d5-bf91-4e54eb38521b
;; a: 32791a69-59f1-404d-9618-37d7d185f236
;; b: 2424d01a-1350-4028-a087-42e981567f8b
;; c: f46ecd75-d889-42de-9731-3ac71c3656a0

(dolist (i '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (xnote-add 0 i)
  (sleep-for 1))

(defun xnote-batch-add (status lst)
  (dolist (data lst)
    (xnote-add status data)
    (sleep-for 1)))
;;;;;;;;;;;;;;;;;;;;;;;;
;; (xnote-batch-add 0 '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
;; (xnote-batch-add 1 '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"))

(xnote-evolve "2" "a")
(xnote-evolve "a" "b")
(xnote-evolve "b" "c")

(xnote-evolve "6" "h")
(xnote-evolve "h" "i")

(xnote-evolve "7" "j")
(xnote-evolve "j" "k")

(xnote-evolve "9" "l")
;;;;;;;;;;;;;;;;;;;;;;;;;
(xnote-query "h")
(xnote-query "i")
(xnote-timeline-data "2023-02-19")

(xnote-db-query [:select * :from note])

(xnote-tree-list (xnote-query "7"))
(xnote-timeline-data "2023-02-19")

(xnote-wrap-plist
 '((1676789206 "ae77d2a7-3b06-40ab-9e08-70e308dd5a91") (1676789207 "748c1233-aa02-43bd-aa35-218d98606711") (1676789208 "e40d27d7-2983-4f20-912a-e384e7a9e3ce") (1676789208 "43c3c07c-e047-445b-80e4-16b1232e439b"))
 :time :id)

(provide 'xnote-test)
