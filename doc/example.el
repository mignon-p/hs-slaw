;; Put this code in your .emacs file so that you can press
;; "C-c c <up>" to insert "ŝ", or "C-c c <down>" to insert "š"

(defun fws-insert-from-slaw ()
  (interactive)
  (insert "ŝ"))

(defun fws-insert-to-slaw ()
  (interactive)
  (insert "š"))

(define-prefix-command               'fws-c-map)
(global-set-key       (kbd "C-c c")  'fws-c-map)
(define-key fws-c-map (kbd "<up>")   'fws-insert-from-slaw)
(define-key fws-c-map (kbd "<down>") 'fws-insert-to-slaw)

;; Put this code in your .emacs file so that .hs-template files
;; will open in Haskell mode.  (See template.txt for more about
;; .hs-template files.)

(add-to-list 'auto-mode-alist '("\\.hs-template$" . haskell-mode))
