;;; q-ts-mode.el --- Treesitter q mode



;;; Code

(require 'q-mode)
(require 'treesit)

(define-derived-mode q-ts-mode prog-mode "Q Script[ts]"
  "Major Mode for editing q scripts with tree-sitter."

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'q)
    (treesit-parser-create 'q)
    (q-ts-setup)))

;; rerun this if it doesn't work
(defun q-ts-setup ()
  "Setup treesit for q-ts-mode"
  ;; tree-sitter setup

  ;; first handle font lock
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     q-ts-font-lock-rules))

  ;; we need to define levels ourselves
  (setq-local treesit-font-lock-feature-list
              '(( comment invalid string ) ( constant number ) () ()))
  ;; indentation rules
  ;;(setq-local treesit-simple-indent-rules q-ts-indent-rules)

  ;; This resets everything
  (treesit-major-mode-setup))

(defvar q-ts-font-lock-rules
  ;; comments
  '(:language q
    :override t
    :feature comment
    ((comment) @font-lock-comment-face
     (comment_block) @font-lock-comment-face)

    ;; invalid atoms that will instantly cause an error
    :language q
    :override t
    :feature invalid
    ((invalid_atom) @font-lock-warning-face)

    :language q
    :override t
    :feature string
    ((string) @font-lock-string-face)

    ;; literals
    :language q
    :feature constant
    ((datetime) @font-lock-constant-face
     (symbol) @font-lock-constant-face
     (symbol_list) @font-lock-constant-face)

    :language q
    :feature number
    ((number) @font-lock-number-face)
    ))

(setq treesit--font-lock-verbose t)
