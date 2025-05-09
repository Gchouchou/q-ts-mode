;;; q-ts-mode.el --- Tree-sitter q mode

;; Author: Justin Yu
;; Homepage: https://github.com/Gchouchou/q-ts-mode
;; Created 14 Jan 2025
;; Version: 0.1

;;; Commentary:

;;; q-mode but using tree-sitter grammar

;;; Requirements:
;;; Package-Requires: ((emacs "29.1") (q-mode))

;;; Code:
(require 'q-mode)
(require 'treesit)

(defconst q-ts-keywords
  (eval-when-compile
    (format "^%s$"
            (regexp-opt
             '("abs" "acos" "asin" "atan" "avg" "bin" "binr" "cor" "cos" "cov" "dev"
               "div" "do" "enlist" "exit" "exp" "getenv" "hopen" "if" "in" "insert" "last"
               "like" "log" "max" "min" "prd" "setenv" "sin" "sqrt" "ss"
               "sum" "tan" "var" "wavg" "while" "within" "wsum" "xexp"))))
  "full match")

(defconst q-ts-builtin-words
  (eval-when-compile
    (format "^\\(?:[.]q[.]\\)?%s$"
            (regexp-opt
             '( "aj" "aj0" "ajf" "ajf0" "all" "and" "any" "asc" "asof" "attr" "avgs" "ceiling"
                "cols" "count" "cross" "csv" "cut" "deltas" "desc"
                "differ" "distinct" "dsave" "each" "ej" "ema" "eval" "except" "fby" "fills"
                "first" "fkeys" "flip" "floor" "get" "group" "gtime" "hclose" "hcount"
                "hdel" "hsym" "iasc" "idesc" "ij" "ijf" "inter" "inv" "key" "keys"
                "lj" "ljf" "load" "lower" "lsq" "ltime" "ltrim" "mavg" "maxs" "mcount" "md5"
                "mdev" "med" "meta" "mins" "mmax" "mmin" "mmu" "mod" "msum" "neg"
                "next" "not" "null" "or" "over" "parse" "peach" "pj" "prds" "prior"
                "prev" "rand" "rank" "ratios" "raze" "read0" "read1" "reciprocal" "reval"
                "reverse" "rload" "rotate" "rsave" "rtrim" "save" "scan" "scov" "sdev" "set" "show"
                "signum" "ssr" "string" "sublist" "sums" "sv" "svar" "system" "tables" "til"
                "trim" "type" "uj" "ujf" "ungroup" "union" "upper" "upsert" "value"
                "view" "views" "vs" "where" "wj" "wj1" "ww" "xasc" "xbar" "xcol" "xcols" "xdesc"
                "xgroup" "xkey" "xlog" "xprev" "xrank")))))

(defconst q-ts-operator-words
  (eval-when-compile
    (format "^%s$"
            (regexp-opt
             '("+"  "*" "%"
               ">" "<" "=" "<>" ">=" "<=""~"
               "_" "#" "$" "!"
               "." "@" "?" "|" "&" ","
               "-")))))

(defvar q-ts--closure-nodes
  (eval-when-compile
    (regexp-opt
     '("func_definition" "progn" "parameter_pass")))
  "A precompiled regex for no reason.")

(defun q-ts--local-var-check (variable)
  "Check if VARIABLE is a local variable."
  (let ((var (treesit-node-text variable t)))
    (unless (or (string-match-p q-keywords var)
                (string-match-p q-ts-builtin-words var)
                (string-match-p q-constant-words var))
      (let ((parent (treesit-parent-until
                           variable
                           ;; func_definition or progn
                           (lambda (node)
                             (let ((type (treesit-node-type node)))
                               (string-match-p (eval-when-compile
                                                 (format
                                                  "^%s$"
                                                  (regexp-opt
                                                  '("func_definition" "progn"))))
                                               type))))))
        (when (string= "func_definition" (treesit-node-type parent))
          (if-let ((param (treesit-node-child-by-field-name parent "parameters")))
              (treesit-filter-child param (lambda (child)
                                            (and (string=
                                                  "variable"
                                                  (treesit-node-type child))
                                                 (string= var (treesit-node-text child))))
                                    t)
            (string-match-p "^\\(x\\|y\\|z\\)$" var)))))))

(defun q-ts--not-func (node)
  "Anti match NODE type with func_definition."
  (not (string= "func_definition" (treesit-node-type node))))

(defvar q-ts-font-lock-rules
  `(;; comments
    :language q
    :feature comment
    ((comment) @font-lock-comment-face
     (comment_block) @font-lock-comment-face)

    :language q
    :feature string
    :override t
    ((string) @font-lock-string-face
     (char) @font-lock-constant-face)

    ;; literals
    :language q
    :feature constant
    :override t
    ((temporal) @font-lock-constant-face
     (symbol) @font-lock-constant-face
     ((symbol) @font-lock-preprocessor-face
      (:match "`:\\(?:\\w\\|[/:._]\\)*" @font-lock-preprocessor-face))
     (symbol_list) @font-lock-constant-face)

    :language q
    :feature number
    :override t
    ((number) @font-lock-number-face
     (byte_list) @font-lock-constant-face)

    ;; system command
    :language q
    :feature command
    ((command) @font-lock-preprocessor-face
     (shell_command) @font-lock-preprocessor-face)

    ;; function parameters
    :language q
    :feature parameter
    ((func_definition
      (parameter_pass
       (variable) @font-lock-variable-name-face)))

    ;; q builtin functions
    :language q
    :feature builtin
    :override t
    ((((variable) @var (:match ,q-ts-builtin-words @var)) @font-lock-builtin-face)
     (((builtin_infix_func) @font-lock-builtin-face (:match ,q-ts-builtin-words @font-lock-builtin-face))))

    ;; q keyword functions
    :language q
    :feature keyword
    :override t
    (((variable) @font-lock-keyword-face (:match ,q-ts-keywords @font-lock-keyword-face))
     (sql_expression [ "distinct" "select" "exec" "delete" "update" "by" "from" "where" ] @font-lock-keyword-face)
     ((builtin_infix_func) @font-lock-keyword-face (:match ,q-ts-keywords @font-lock-keyword-face)))

    ;; q operator functions
    :language q
    :feature operator
    :override t
    (((builtin_infix_func) @font-lock-operator-face (:match ,q-ts-operator-words @font-lock-operator-face))
     (assignment_func) @font-lock-operator-face)
    ;; q builtin constants
    :language q
    :feature q-constant
    :override t
    (((variable) @var (:match ,q-constant-words @var)) @font-lock-constant-face)

    ;; function application
    :language q
    :feature function-call
    ((func_app
      function: (variable
                 element: (identifier) @font-lock-function-call-face)))

    ;; infix modified function
    :language q
    :feature infix-mod
    ((infix_mod_func
      modifier: (_) @font-lock-function-call-face))

    ;; escape sequences
    :language q
    :feature escape-char
    :override t
    ((escape_sequence) @font-lock-preprocessor-face)

    ;; q local variables, very expensive
    :language q
    :feature local-variable
    (((variable) @font-lock-variable-name-face
      (:pred q-ts--local-var-check @font-lock-variable-name-face)))

    ;; invalid atoms that will instantly cause an error
    :language q
    :feature invalid
    :override t
    ((invalid_atom) @font-lock-warning-face)

    ;; highlighting on function and variable assignment
    ;; this is pretty useless
    :language q
    :feature assignment
    ((func_app
      parameter1: (variable) @font-lock-function-name-face
      function:  (assignment_func)  @font-lock-operator-face
      parameter2: (func_definition))
     (func_app
      function:  (assignment_func)  @font-lock-operator-face
      parameters: (parameter_pass :anchor
                   (variable) @font-lock-function-name-face
                   :anchor (semicolon) :anchor
                   (func_definition) :anchor))
     ((func_app
      parameter1: (variable) @font-lock-variable-name-face
      function: (assignment_func) @font-lock-operator-face
      parameter2: (_) @value)
      (:pred q-ts--not-func @value))
     ((func_app
       function: (assignment_func)  @font-lock-operator-face
       parameters: (parameter_pass :anchor
                    (variable) @font-lock-variable-name-face
                    :anchor (semicolon) :anchor
                    (_) @value :anchor))
      (:pred q-ts--not-func @value)))))

(defvar q-ts--indent-rules
  `((q
     ;; last comment block has to be 0 aligned
     ;; might as well make them all 0 aligned
     ;; program and progn should always be 0 aligned
     ((parent-is ,(format
                   "^%s$"
                   (regexp-opt
                    '("comment_block" "program" "progn"))))
      column-0 0)
     ;; table columns look nicer without the indent
     ((parent-is ,(format
                  "^%s$"
                  (regexp-opt
                   '("table_columns" "func_body"))))
      parent 0)
     ;; matches multiline nodes e.g. comments and shell command
     (no-node parent-bol q-ts--check-syscmd)
     ;; default indent
     (catch-all parent ,q-indent-step))))

(defun q-ts--check-syscmd (node parent bol)
  "Return 0 if not in shell command node else return `q-indent-step'.

NODE is nil.
PARENT is nil.
BOL is the position."
  (if (string= "shell_command" (treesit-node-type (treesit-node-at bol)))
      q-indent-step
    0))

(defvar q-ts--syntax-query
  (treesit-query-compile 'q
                         '((string) @string
                           (char) @string
                           (comment) @comment
                           (comment_block) @comment)))

(defun q-ts--syntax-propertize (beg end)
  (let ((captures (treesit-query-capture 'q
                                         q-ts--syntax-query
                                         beg end)))
    (pcase-dolist (`(,name . ,node) captures)
      (let* ((ns (treesit-node-start node))
             (ne (treesit-node-end node))
             (syntax (pcase-exhaustive name
                       ('string (string-to-syntax "|"))
                       ('comment (string-to-syntax "!")))))
        (put-text-property ns (1+ ns) 'syntax-table syntax)
        (put-text-property (1- ne) ne 'syntax-table syntax)))))

;; rerun this if it doesn't work
(defun q-ts-setup ()
  "Setup treesit for q-ts-mode."
  (interactive)
  ;; tree-sitter setup

  ;; first handle font lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     q-ts-font-lock-rules))

  ;; comments
  (setq-local comment-start q-comment-start)
  (setq-local comment-start-skip "\\(^\\|[ \t]\\)\\(/+[ \t]*\\)")
  (setq-local comment-end "")

  (setq-local syntax-propertize-function #'q-ts--syntax-propertize)

  ;; we need to define levels ourselves
  (setq-local treesit-font-lock-feature-list
              '(( comment string ) ( constant number builtin keyword )
                ( parameter command q-constant function-call escape-char operator infix-mod )
                ( local-variable invalid assignment )))

  ;; indentation rules
  (setq-local treesit-simple-indent-rules q-ts--indent-rules)

  ;; This resets everything
  (treesit-major-mode-setup))

(defvar q-ts--strip-query
  (treesit-query-compile
   'q
   '((comment) @comment
     (comment_block) @comment_block
     (newline_extra) @newline_extra
     (shebang) @shebang)))

(defun q-ts-strip (text)
  "Strip TEXT of all comments, collapse expressions.
Analog to `q-strip' but leverages tree-sitter."
  (with-temp-buffer
    (insert text)
    (let* ((shift 0)
           (capture (treesit-query-capture
                     'q
                     q-ts--strip-query
                     nil
                     nil
                     t))
           ;; TODO: try deleting bounds directly instead of accumulating then deleting
           (bounds (mapcar
                    (lambda (node)
                      (cons (treesit-node-start node) (treesit-node-end node)))
                    capture)))
      (pcase-dolist (`(,start . ,end) bounds)
        (delete-region (- start shift)
                       (- end shift))
        (setq shift (+ shift (- end start)))))
    ;; do regex substitution of superfluous spaces
    (goto-char (point-min))
    (while (re-search-forward "[ \t]\\{2,\\}" nil t)
      (let* ((space-count (length (match-string 0)))
             ;; move backwards one space for better position
             (node (treesit-node-at (- (point) 1) 'q)))
        (unless (string= "string_fragment" (treesit-node-type node))
          ;; leave one space
          (delete-region (- (point) space-count -1)
                         (point)))))
    ;; delete trailing white spaces
    (goto-char (point-max))
    (delete-blank-lines)
    (while (eq (char-before) ?\n)
      (delete-char -1))
    (buffer-string)))

;; override q-strip
(advice-add 'q-strip :override #'q-ts-strip)

(defun q-ts-eval-extended-line ()
  "Send the full line to the inferior q[con] process using tree-sitter."
  (interactive)
  (let ((node (treesit-parent-until
         (treesit-node-at (point))
         (lambda (node)
           (string= "program"
                    (treesit-node-type (treesit-node-parent node))))
         t)))
    (if (and node (string-match-p "^\\(progn\\|system_command\\)$"
                                  (treesit-node-type node)))
        (q-eval-region (treesit-node-start node) (treesit-node-end node))
      (error "No extended line expression found"))))

;;TODO maybe override eval thing at point and eval function

(defvar q-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map q-mode-map)
    (define-key map "\C-c\C-e" 'q-ts-eval-extended-line)
    map))

;;;###autoload
(define-derived-mode q-ts-mode prog-mode "Q Script[ts]"
  "Major Mode for editing q scripts with tree-sitter."
  :syntax-table q-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'q)
    (treesit-parser-create 'q)
    (q-ts-setup))
  ;; Do not edit k files with treesitter mode
  (add-to-list 'auto-mode-alist '("\\.q\\'" . q-ts-mode)))

(provide 'q-ts-mode)

;;; q-ts-mode.el ends here
