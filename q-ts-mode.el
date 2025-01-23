;;; q-ts-mode.el --- Treesitter q mode

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

(defun q-ts--anti-assignment (node)
  "Anti matches assignment NODE."
  (not
   (and (string= "func_app"
                 (treesit-node-type node))
        (when-let* ((func (treesit-node-child-by-field-name node "function"))
                    (type (treesit-node-type func)))
          (or (string= "no_comma_assignment_func" type)
              (string= "assignment_func" type)))
        (when-let* ((param1 (treesit-node-child-by-field-name node "parameter1")))
          (string= "variable" (treesit-node-type param1))))))

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
                                                  '("func_definition" "progn" "parameter_pass"))))
                                               type))))))
        (if-let ((param (and (string= "func_definition"
                                      (treesit-node-type parent))
                             (treesit-node-child-by-field-name parent "parameters"))))
            (treesit-filter-child param (lambda (child)
                                          (and (string=
                                                "variable"
                                                (treesit-node-type child))
                                               (string= var (treesit-node-text child))))
                                  t)
          (string-match-p "^\\(x\\|y\\|z\\)$" var))
        ))))

(defun q-ts--not-func (node)
  "Anti match NODE type with func_definition."
  (not (string= "func_definition" (treesit-node-type node))))

;;;###autoload
(define-derived-mode q-ts-mode prog-mode "Q Script[ts]"
  "Major Mode for editing q scripts with tree-sitter."
  :syntax-table q-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'q)
    (treesit-parser-create 'q)
    (q-ts-setup)))

;; rerun this if it doesn't work
(defun q-ts-setup ()
  "Setup treesit for q-ts-mode."
  (interactive)
  ;; tree-sitter setup

  ;; first handle font lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     q-ts-font-lock-rules))

  ;; we need to define levels ourselves
  (setq-local treesit-font-lock-feature-list
              '(( comment string ) ( constant number builtin keyword )
                ( parameter command q-constant function-call escape-char operator infix-mod )
                ( local-variable invalid output assignment )))

  ;; indentation rules
  (setq-local treesit-simple-indent-rules
              `((q
                 ;; last comment block has to be 0 aligned
                 ;; might as well make them all 0 aligned
                 ((parent-is "comment_block") column-0 0)
                 ;; some insane query to catch multiline commands
                 ;;((query (((progn output: (_)) :anchor (newline_extra) :anchor (_) @catch))) parent ,q-indent-step)
                 ;; all the closers are on same line as inside
                 ((parent-is "parameter_pass") parent-bol ,q-indent-step)
                 ((parent-is "parenthesis_exp") parent-bol ,q-indent-step)
                 ((parent-is "definition") parent-bol ,q-indent-step)
                 ((parent-is "table_keys") parent ,q-indent-step)
                 ((parent-is "list") parent ,q-indent-step)
                 ((parent-is "program") column-0 0)
                 ;; progn is start of line
                 ((node-is "progn") parent-bol 0)
                 ;; no indent at start of program
                 ((parent-is "func_app") parent-bol q-ts--check-indent)
                 ;; semicolon should keep indentation
                 ((node-is "newline_extra") parent-bol 0)
                 ;; default
                 (no-node parent-bol q-ts--check-syscmd)
                 ((query ((_ (_) @child ))) parent 0)
                 )))

  ;; This resets everything
  (treesit-major-mode-setup))

(defun q-ts--check-indent (node parent bol)
  "Return 0 if parent line is already indented or `q-indent-step' otherwise.

NODE is the node that should be indented.

PARENT is the parent of NODE.
BOL is position of buffer."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (goto-char (pos-bol))
    (if (string-match-p "[ \t]" (buffer-substring (point) (+ (point) 1)))
        0
      q-indent-step)))

(defun q-ts--check-syscmd (node parent bol)
  "Return 0 if not in shell command node else return `q-indent-step'.

NODE is nil.
PARENT is nil.
BOL is the position."
  (if (string= "shell_command"(treesit-node-type (treesit-node-at bol)))
      q-indent-step
    0))

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
      function: (variable
                element: (identifier) @font-lock-function-call-face)
      modifier: (_) @font-lock-preprocessor-face))

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
    ((invalid_atom) @font-lock-warning-face
     ((variable) @font-lock-warning-face
      (:match ,(format
                "^%s$"
                (regexp-opt
                 '("select" "exec" "from" "by" "update" "delete")))
              @font-lock-warning-face)))

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
      (:pred q-ts--not-func @value)))

    ;; last expression in progn but does not match assignment
    :language q
    :feature output
    :override append
    (((progn
       output: (_) @underline)
      (:pred q-ts--anti-assignment @underline)))
    ))

(setq treesit--font-lock-verbose t)
(setq treesit--indent-verbose nil)

(provide 'q-ts-mode)
;;; q-ts-mode.el ends here
