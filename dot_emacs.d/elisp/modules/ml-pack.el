;; -*- lexical-binding: t; -*-

;; Set package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")))

;; Initialize the package manager
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq
 use-package-always-ensure t
 use-package-verbose t)

(provide 'ml-pack)
