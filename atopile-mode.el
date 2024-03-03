;;; atopile-mode.el --- Major mode for editing .ato files

;; Author: William Emfinger <waemfinger@gmail.com>
;; Version: 1.0
;; Keywords: languages ato atopile

;;; Commentary:
;;
;; This is a simple major mode for editing .ato files.

(defvar atopile-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Define # as the beginning of a comment
    (modify-syntax-entry ?# "<" syntax-table)
    ;; Define newline as the end of a comment
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `atopile-mode'.")

;; ;;; Code:
(defvar atopile-font-lock-keywords
  `(
    ("= \\([+-]?[0-9]+\\(\\.[0-9]+\\)?\\)" . (1 font-lock-warning-face))
    ;; match [fpnuµmkKMG](ohm|F|H|Hz|A|V|W|S)\b and highlight the word
    ("\\([fpnuµmkKMG]\\(ohm\\|F\\|H\\|Hz\\|A\\|V\\|W\\|S\\)\\)\\b" . (1 font-lock-constant-face))
    ;; match +/- [0-9]+% and highlight the word
    ("\\(+/- [0-9]+\\%\\)" . (1 font-lock-doc-face))


    ("\\b\\(import\\|module\\|component\\|to\\|from\\|assert\\|pin\\|signal\\|new\\)\\b" . (1 font-lock-keyword-face))
    ("\\.\\(footprint\\|value\\|pin\\|signal\\|mpn\\|configure\\|add\\|pad\\|out\\|in\\|pin\\|ground\\|power\\|gnd\\|vcc\\)\\($\\| \\)" . (1 font-lock-keyword-face))
    ("\\b\\(with\\)\\b" . (1 font-lock-keyword-face)) ;; keyword.control.statement.ato

    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" . (1 font-lock-variable-name-face))
    ("\\(\"[^\"]*\"\\)" . (1 font-lock-string-face))
    ;; ("\\<\\([+-]?[0-9]+\\(\\.[0-9]+\\)?\\)\\([uµnpfFmMkKMG]?(ohm|F|H|Hz|A|V|W|S)?\\)\\( +/- [0-9]+%\\)?\\>" . (1 font-lock-constant-face))
    ("\\<\\(true\\|false\\)\\>" . (1 font-lock-constant-face))
    ;; Match comments, ensuring they are highlighted even when inline with code
    ("\\(#.*\\)" . (1 font-lock-comment-face)))
  "Keyword highlighting for `atopile-mode'.")

;;;###autoload
(define-derived-mode atopile-mode prog-mode "atopile"
  "Major mode for editing .ato files."
  :syntax-table atopile-mode-syntax-table
  (setq font-lock-defaults '(atopile-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ato\\'" . atopile-mode))

(provide 'atopile-mode)
;;; atopile-mode.el ends here
