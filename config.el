;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq doom-font (font-spec :family "NasuM" :size 14)
      doom-variable-pitch-font (font-spec :family "Nasu")
      doom-unicode-font (font-spec :family "NasuM")
      doom-big-font (font-spec :family "NasuM" :size 22))
(setq doom-one-brighter-comments t)
;; don't lighten the background of the comment
(setq doom-one-comment-bg nil)
(setq-default comment-start "# ")

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Disable trailing whitespace highlighting when in some modes
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

;; Highlight tab, hard space, and full-width space
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

;; Disable auto-fill-mode when in markdown mode
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode -1)))

;; Enable soft wrap
(global-visual-line-mode t)

;; Resize window quickly
;; http://d.hatena.ne.jp/khiker/20100119/window_resize
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

;; switch between window with C-hjkl
(map! :nvi "C-h" #'evil-window-left
      :nvi "C-j" #'evil-window-down
      :nvi "C-k" #'evil-window-up
      :nvi "C-l" #'evil-window-right)
;; move the window with C-S-hjkl
(map! :nvi "C-S-h" #'+evil/window-move-left
      :nvi "C-S-j" #'+evil/window-move-down
      :nvi "C-S-k" #'+evil/window-move-up
      :nvi "C-S-l" #'+evil/window-move-right)

;; minimal number of screen lines to keep above and below the cursor
(setq-default scroll-margin 3)
;; minimal number of screen columns to keep to the left and to the right of the cursor
(setq-default hscroll-margin 5)

;; delete character without yanking
(map! :n "x" #'delete-char)

;; Don't do square-bracket space-expansion where it doesn't make sense to
(after! smartparens-text
  (sp-local-pair 'text-mode
                 "[" nil :post-handlers '(:rem ("| " "SPC"))))

;; Make C-d usable in insert mode
(map! :i "C-d" #'delete-char)

;; org-mode
;; log when a certain todo item was finished
(setq org-log-done t)

;; Make sure that the weekdays in the time stamps of your Org mode files and in the agenda appear in English.
(setq system-time-locale "C")

(setq org-agenda-files (list "~/Dropbox2/Dropbox/org/todo.org"))

;; https://stackoverflow.com/questions/20164918/how-to-untick-checkboxes-in-org-mode-for-the-next-cyclic-repetitive-task
(require 'org-checklist)
