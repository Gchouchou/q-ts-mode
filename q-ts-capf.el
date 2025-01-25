;;; Code:
(require 'q-capf)

(defun q-ts-capf--bounds ()
  "Return bounds of variable in q gramma."
  (let* ((node (treesit-node-at (point) 'q))
         (parent (when node (treesit-node-parent node))))
    (if (and parent
             (string= "variable" (treesit-node-type parent))
             (<= (treesit-node-start parent) (point) (treesit-node-end parent)))
        (cons (treesit-node-start parent) (treesit-node-end parent))
      ;; call q-capf--bounds as backup
      (q-capf--bounds))))

;; override q-capf--bounds with treesitter version
;;(advice-add 'q-capf--bounds :override 'q-ts-capf--bounds)

(defun q-ts-table-col-capf ()
  "Completion at point for table column names"
  ;; we need to make sure we have a sql expression
  ;; handle edge case at end of line
  (when-let* ((node (treesit-node-at (point) 'q))
              (sql_exp (treesit-parent-until
                       node
                       (lambda (node)
                         (string= "sql_expression"
                                  (treesit-node-type node)))
                       t))
              (a (message "%s" sql_exp))
              (table-node (treesit-node-child-by-field-name sql_exp "table"))
              ;; only continue if table is a variable
              (table (and (string= "variable" (treesit-node-type table-node))
                          (treesit-node-text table-node)))
              ;; double check we have documentation and more than 1 column for table
              (element (treesit-node-text (treesit-node-child-by-field-name table-node "element")))
              (namespace (substring table 0 (- (length table) (length element))))
              (doc (q-capf-get-doc element namespace))
              (columns (gethash "cols" doc))
              ;; get the nodes/positions of all keywords
              (keyword-children (treesit-filter-child
                                 sql_exp
                                 (lambda (node)
                                   (string-match-p
                                    "^\\(select\\|exec\\|delete\\|update\\|by\\|from\\|where\\)$"
                                    ;; type works on exact matches
                                    (treesit-node-type node)))))
              (keyword (pop keyword-children))
              (status 'init))
        (message "%s" keyword-children)
    (while (and (eq status 'init) keyword-children)
      (let ((next-word (pop keyword-children)))
        (message "%s" next-word)
        (setq status (cond
                         ;; catch special case between from and where
                         ((string= "where" (treesit-node-type next-word))
                          (< (treesit-node-end next-word) (point)))
                         ((< (treesit-node-end keyword) (point) (treesit-node-start next-word)) t)
                         ((< (point) (treesit-node-end keyword)) 'invalid)
                         (t 'init)))))
    ;; only succeed when inbetween is t
    (when (eq status t)
      (let ((bounds (q-ts-capf--bounds)))
        (list
         (car bounds)
         (cdr bounds)
         (append columns nil)
         :exclusive 'no
         :annotation-function
         (lambda (col) " table column")
         )))
    ))

(provide 'q-ts-capf)
;;; q-ts-capf.el ends here
