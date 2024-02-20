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
                                   ("\\b\\(import\\|from\\)" . (1 font-lock-keyword-face))
                                   ("\\.\\(footprint\\|value\\|pin\\|signal\\|mpn\\|configure\\|add\\|pad\\|out\\|in\\|pin\\|ground\\|power\\|gnd\\|vcc\\|footprint\\|mpn\\)\\($\\| \\)" . (1 font-lock-keyword-face))
                                   ("\\b\\(import\\|module\\|component\\|to\\|from\\|assert\\|new\\)\\b" . (1 font-lock-keyword-face))
                                   ("\\b\\(with\\)" . (1 font-lock-keyword-face)) ;; keyword.control.statement.ato
                                   ("\\(^\\s*\\(module\\|component\\|interface\\)\\s+\\([a-zA-Z_][a-zA-Z_0-9]*\\)\\)" . (1 font-lock-keyword-face)) ;; meta.class.ato - storage.type.class.ato entity.name.type.class.ato
                                   ;; ("\\b\\(new\\)\\s+\\(\\w+\\(?:\\.\\w*\\)?\\)" . (1 font-lock-keyword-face)) ;; keyword.operator.new.ato, entity.name.type.instance.ato
                                   ("\\b\\(pin\\|signal\\)" . (1 font-lock-keyword-face)) ;; storage.type.c
                                   ("\\b\\(assert\\)" . (1 font-lock-keyword-face)) ;; keyword.control.ato
                                   ("\\b\\(if\\|while\\|for\\|return\\|optional\\)" . (1 font-lock-keyword-face)) ;; keyword.control.ato
                                   ("\"[^\"]*\"" . font-lock-string-face)
                                   ("\\b\\(True\\|False\\)" . (1 font-lock-constant-face))
                                   ("\\([fpnuµmkKMG]\\(ohm\\|F\\|H\\|Hz\\|A\\|V\\|W\\|S\\)\\)\\b" . (1 font-lock-constant-face))
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
