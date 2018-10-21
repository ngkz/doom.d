;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-font (font-spec :family "NasuM" :size 14)
      doom-variable-pitch-font (font-spec :family "Nasu")
      doom-unicode-font (font-spec :family "NasuM")
      doom-big-font (font-spec :family "NasuM" :size 22))
(setq doom-one-brighter-comments t)
(setq-default comment-start "# ")

(setq-default show-trailing-whitespace t)

;; https://qiita.com/tadsan/items/df73c711f921708facdc
(defun my/disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil))

(defvar my/disable-trailing-modes
  '(comint-mode
    eshell-mode
    eww-mode
    term-mode
    twittering-mode))

(mapc
 (lambda (mode)
   (add-hook (intern (concat (symbol-name mode) "-hook"))
             'my/disable-trailing-mode-hook))
 my/disable-trailing-modes)

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

;soft wrapping
(global-visual-line-mode t)

;resize window quickly
;http://d.hatena.ne.jp/khiker/20100119/window_resize
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally (* dx 5)))
              ((= c ?L)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally (* dx 5)))
              ((= c ?H)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window (* dy 2)))
              ((= c ?J)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window (* dy 2)))
              ((= c ?K)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(map! :map evil-window-map
      "SPC" #'my-window-resizer) ; CTRL-w SPC or SPC w SPC

(map! :nvime "C-;" #'avy-goto-char-2)
