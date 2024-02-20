;;; atopile-mode.el --- Major mode for editing .ato files

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Keywords: languages ato

;;; Commentary:
;;
;; This is a simple major mode for editing .ato files.

;;; Code:

(defvar atopile-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Add syntax highlighting for comments
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `atopile-mode'.")

(defvar atopile-font-lock-keywords
  '(("\\<\\(import\\|module\\|component\\|from\\|new\\|footprint\\|value\\|pin\\|mpn\\|configure\\|add\\|pad\\|out\\|pin\\|ground\\|power\\|switch\\|footprint\\|mpn\\)\\>" . font-lock-keyword-face)
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" . font-lock-variable-name-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("\\<\\([0-9]+\\(\\.[0-9]+\\)?\\)\\(uF\\|pF\\|nF\\|mF\\|%\\|k\\|M\\|G\\)\\>" . font-lock-constant-face)
    ("\\<\\([+-]?[0-9]+\\)\\>" . font-lock-constant-face)
    ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face))
  "Keyword highlighting for `atopile-mode'.")

(define-derived-mode atopile-mode prog-mode "Atopile"
  "Major mode for editing .ato files."
  :syntax-table atopile-mode-syntax-table
  (setq font-lock-defaults '(atopile-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ato\\'" . atopile-mode))

(provide 'atopile-mode)
;;; atopile-mode.el ends here
