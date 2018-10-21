;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-font (font-spec :family "NasuM" :size 14)
      doom-variable-pitch-font (font-spec :family "Nasu")
      doom-unicode-font (font-spec :family "NasuM")
      doom-big-font (font-spec :family "NasuM" :size 22))
(setq doom-one-brighter-comments t)
(setq-default comment-start "# ")
(setq-default show-trailing-whitespace t)

; highlight tab, hard space, and full-width space
(require 'whitespace)
(setq whitespace-style '(
    face
    tabs
    spaces
    space-mark
    tab-mark
))
(setq whitespace-display-mappings '(
    (space-mark ?\u3000 [?␣])       ;full-width space AMBIGUOUS WIDTH!
    (space-mark ?\u00A0 [?\uFF65])   ;hard space
    (tab-mark   ?\t     [?» ?\t])    ;tab
))
(setq whitespace-space-regexp "\\([\u3000]+\\)") ; highlight only full-width space
(global-whitespace-mode t)

;turn off auto-fill
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode -1)))
