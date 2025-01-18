;;; q-ts-mode.el --- Treesitter q mode



;;; Code

(require 'q-mode)
(require 'treesit)

(defconst q-ts-keywords
  (format "^%s$"
          (regexp-opt
           '("abs" "acos" "asin" "atan" "avg" "bin" "binr" "cor" "cos" "cov" "dev"
             "div" "do" "enlist" "exit" "exp" "getenv" "hopen" "if" "in" "insert" "last"
             "like" "log" "max" "min" "prd" "setenv" "sin" "sqrt" "ss"
             "sum" "tan" "var" "wavg" "while" "within" "wsum" "xexp") t)))

(defconst q-ts-builtin-words
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
                "xgroup" "xkey" "xlog" "xprev" "xrank") t)))


(define-derived-mode q-ts-mode prog-mode "Q Script[ts]"
  "Major Mode for editing q scripts with tree-sitter."
  :syntax-table q-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'q)
    (treesit-parser-create 'q)
    (q-ts-setup)))

;; rerun this if it doesn't work
(defun q-ts-setup ()
  "Setup treesit for q-ts-mode"
  (interactive)
  ;; tree-sitter setup

  ;; first handle font lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     q-ts-font-lock-rules))

  ;; we need to define levels ourselves
  (setq-local treesit-font-lock-feature-list
              '(( comment string ) ( constant number builtin keyword )
                ( parameter command q-constant ) ( invalid output assignment )))
  ;; indentation rules
  ;;(setq-local treesit-simple-indent-rules q-ts-indent-rules)

  ;; This resets everything
  (treesit-major-mode-setup))

(defvar q-ts-font-lock-rules
  `(;; comments
    :language q
    :feature comment
    ((comment) @font-lock-comment-face
     (comment_block) @font-lock-comment-face)

    ;; invalid atoms that will instantly cause an error
    :language q
    :feature invalid
    :override t
    ((invalid_atom) @font-lock-warning-face
     ((variable) @font-lock-warning-face
      (:match "\\(select\\|exec\\|from\\|by\\|update\\|delete\\)" @font-lock-warning-face)))

    :language q
    :feature string
    ((string) @font-lock-string-face)

    ;; literals
    :language q
    :feature constant
    ((datetime) @font-lock-constant-face
     ((symbol) @font-lock-preprocessor-face
      (:match "`:\\(?:\\w\\|[/:._]\\)*" @font-lock-preprocessor-face))
     (symbol) @font-lock-constant-face
     (symbol_list) @font-lock-constant-face)

    :language q
    :feature number
    ((number) @font-lock-number-face)

    ;; highlighting on function assignment
    :language q
    :feature assignment
    ((func_app
      parameter1: (variable) @font-lock-function-name-face
      function: (assignment_func) @font-lock-operator-face
      parameter2: (func_definition))
     ;; note the anchor to enforce bivariate
     (func_app
      function: (assignment_func) @font-lock-operator-face
      parameters: (parameter_pass
                   (variable) @font-lock-function-name-face
                   :anchor ";" :anchor
                   (func_definition) :anchor))
     ;; not a function
     ((func_app
       function: (assignment_func) @font-lock-operator-face
       parameters: (parameter_pass
                    (variable) @font-lock-variable-name-face
                    :anchor ";" :anchor
                    (_) :anchor)))
     ((func_app
       parameter1: (variable) @font-lock-variable-name-face
       function: (assignment_func) @font-lock-operator-face
       parameter2: (_))))

    ;; system command
    :language q
    :feature command
    ((command) @font-lock-constant-face
     (shell_command) @font-lock-constant-face)

    ;; q builtin functions
    :language q
    :feature builtin
    (((variable) @var (:match ,q-ts-builtin-words @var)) @font-lock-builtin-face
     ((no_comma_regular_infix_func) @f (:match ,q-ts-builtin-words @f)) @font-lock-builtin-face)

    ;; q keyword functions
    :language q
    :feature keyword
    :override t
    (((variable) @var (:match ,q-ts-keywords @var)) @font-lock-keyword-face
     (sql_expression [ "select" "exec" "delete" "update" "by" "from" "where" ] @font-lock-keyword-face)
     ((no_comma_regular_infix_func) @f (:match ,q-ts-keywords @f)) @font-lock-keyword-face)

    ;; q builtin constants
    :language q
    :feature q-constant
    (((variable) @var (:match ,q-constant-words @var)) @font-lock-constant-face)

    ;; last expression in functions and progn
    :language q
    :feature output
    :override append
    ((progn
       output: (_) @underline))
    )
  )

(setq treesit--font-lock-verbose t)

(defun ts-q ()
  (interactive)
  (let ((parser (treesit-parser-create 'q)))
    (message "%s"
             (treesit-query-capture
              (treesit-buffer-root-node)
              '((func_definition
                 output: (_) @a)
                (progn
                  output: (_) @b))))))

(provide 'q-ts-mode)
;;; q-ts-mode.el ends here
