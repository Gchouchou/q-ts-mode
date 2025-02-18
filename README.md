# q-ts-mode

q-mode with [treesitter](https://tree-sitter.github.io/tree-sitter/) support.

These files are barebone sample configs of treesitter features and the treesitter-q
parser. Customization will be necessary.

## Dependencies

This package has the following dependencies:
- treesitter parser of the kdb/q language: https://github.com/Gchouchou/tree-sitter-q
- [q-mode](https://github.com/psaris/q-mode)

The q-ts-capf file requires https://github.com/Gchouchou/q-capf
for a table column name completion.

# Overriding q-mode

To override `q-mode` with `q-ts-mode` you can add the following for the init file.

``` emacs-lisp
(add-to-list 'auto-mode-alist '("\\.q\\'" . q-ts-mode))
```

You can also use `major-mode-remap-alist`:

``` emacs-lisp
(setq major-mode-remap-alist
      '((q-mode . q-ts-mode)))
```

If you are also using the [ob-q](https://github.com/Gchouchou/ob-q) package,
you might want to override the q language blocks with treesitter mode.
You can achieve this by customizing `org-src-lang-modes`. Add the following
to config file:

``` emacs-lisp
(add-to-list 'org-src-lang-modes '("q" . q-ts-mode)
```

