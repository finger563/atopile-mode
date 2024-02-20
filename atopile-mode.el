;;; atopile-mode.el --- Major mode for editing .ato files

;; Author: William Emfinger <waemfinger@gmail.com>
;; Version: 1.0
;; Keywords: languages ato atopile

;;; Commentary:
;;
;; This is a simple major mode for editing .ato files.

;;; Code:
(defvar atopile-font-lock-keywords)
(setq atopile-font-lock-keywords `(
                                   ("\\b\\(import\\|module\\|component\\|to\\|from\\|assert\\|new\\)\\b" . (1 font-lock-keyword-face))
                                   ;; match .(footprint|value|pin|signal|mpn|configure|add|pad|out|in|pin|ground|power|gnd|vcc) and highlight the word
                                   ("\\.\\(footprint\\|value\\|pin\\|signal\\|mpn\\|configure\\|add\\|pad\\|out\\|in\\|pin\\|ground\\|power\\|gnd\\|vcc\\)\\($\\| \\)" . (1 font-lock-keyword-face))
                                   ;; match \b(with)\b and highlight the word
                                   ("\\b\\(with\\)\\b" . (1 font-lock-keyword-face)) ;; keyword.control.statement.ato
                                   ;; match \b(module|component|interface)\b and highlight the word
                                   ("\\(^\\s*\\(module\\|component\\|interface\\)\\s+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\)" . (1 font-lock-keyword-face)) ;; meta.class.ato - storage.type.class.ato entity.name.type.class.ato
                                   ;; ("\\b\\(new\\)\\s+\\(\\w+\\(?:\\.\\w*\\)?\\)" . (1 font-lock-keyword-face)) ;; keyword.operator.new.ato, entity.name.type.instance.ato
                                   ;; match \b(pin|signal)\b and highlight the word
                                   ("\\b\\(pin\\|signal\\)" . (1 font-lock-keyword-face)) ;; storage.type.c
                                   ;; match \b(assert) and highlight the word
                                   ("\\b\\(assert\\)" . (1 font-lock-keyword-face)) ;; keyword.control.ato
                                   ("\\b\\(if\\|while\\|for\\|return\\|optional\\)" . (1 font-lock-keyword-face)) ;; keyword.control.ato
                                   ("\"[^\"]*\"" . font-lock-string-face)
                                   ("\\b\\(True\\|False\\)" . (1 font-lock-constant-face))
                                   ;; match [fpnuµmkKMG](ohm|F|H|Hz|A|V|W|S)\b and highlight the word
                                   ("\\([fpnuµmkKMG]\\(ohm\\|F\\|H\\|Hz\\|A\\|V\\|W\\|S\\)\\)\\b" . (1 font-lock-constant-face))
                                   ;; match +/- [0-9]+% and highlight the word
                                   ("\\(+/- [0-9]+\\%\\)" . (1 font-lock-doc-face))
                                   ;; ("\\([0-9]+\\(\\.[0-9]+\\)?\\)\\([a-zA-Z]+\\|\\%\\)?" . (1 font-lock-type-face)) ;; constant.numeric.ato
                                   ("= \\([+-]?[0-9]+\\(\\.[0-9]+\\)?\\)" . (1 font-lock-warning-face))
                                   ("\\([A-Za-z_][A-Za-z0-9_]*\\)" . (1 font-lock-variable-name-face))
                                   ("[[:xdigit:]]+" . (1 font-lock-type-face))
                                   ("-?[[:digit:]]+" . (1 font-lock-type-face))
                                   )
)

;;;###autoload
(define-derived-mode atopile-mode fundamental-mode
  "atopile"
  "Major mode for editing Atopile (.ato) files."
  (setq font-lock-defaults '((atopile-font-lock-keywords)))
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ato\\'" . atopile-mode))

(provide 'atopile-mode)
;;; atopile-mode.el ends here
