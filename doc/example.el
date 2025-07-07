;; Put this code in your .emacs file so that you can press
;; "C-x <up>" to insert "ŝ", or "C-x <down>" to insert "š"

(defun fws-insert-from-slaw ()
  (interactive)
  (insert "ŝ"))

(defun fws-insert-to-slaw ()
  (interactive)
  (insert "š"))

(global-set-key (kbd "C-x <up>")   'fws-insert-from-slaw)
(global-set-key (kbd "C-x <down>") 'fws-insert-to-slaw)

;; Put this code in your .emacs file so that .hs-template files
;; will open in Haskell mode.  (See template.txt for more about
;; .hs-template files.)

(add-to-list 'auto-mode-alist '("\\.hs-template$" . haskell-mode))
