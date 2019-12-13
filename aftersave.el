;;; aftersave.el --- Convenient way to add after-save hooks on the fly

;; Copyright (C) 2019 Roman Gonzalez

;; Author: Roman Gonzalez <open-source@roman-gonzalez.info>
;; Mantainer: Roman Gonzalez <open-source@roman-gonzalez.info>
;; Created: 13 Dec 2019
;; Keywords: Hook
;; Version: 0.0.1

;; Code inspired by ideas from Tavis Rudd

;; This file is not part of GNU Emacs.

;; This file is free software (MIT License)


;;; Commentary:
;;
;; This package helps removing manual steps in the development cycle. If you
;; need to execute the same commands after a save (run tests, restart browser,
;; etc.), this package allows you to add an Emacs function in the
;; after-save-hook of the editor (or the local buffer).

;;; Code:

(defun aftersave/-get-hook-funcs (&optional local)
  "Return all the functions that are associated to the `after-save-hook'.

`LOCAL' tells the function to use the buffer-local version of the
hook."
  (let ((hook
         (if local
             after-save-hook
           ;; else get global value
           (with-temp-buffer
             after-save-hook))
         ))
    (delq nil (mapcar
               (lambda (e) (if (symbolp e) e))
               hook))))

(defun aftersave/-get-all-hooks ()
  (let (hlist (list))
    (mapatoms (lambda (a)
                (if (and (not (null (string-match ".*-hook"
                                                  (symbol-name a))))
                         (not (functionp a)))
                    (add-to-list 'hlist a))))
    hlist))

(defun aftersave/-get-hook-func-names (&optional local)
  "Get all the names of the functions associated to the `after-save-hook'.

`LOCAL' tells the function to use the buffer-local version of the
hook."
  (mapcar 'symbol-name (aftersave/-get-hook-funcs local)))


(defun aftersave/-remove-hook (fname &optional local)
  "Remove a callback function from the `after-save-hook'.

`FNAME' is the name of the function, and `LOCAL' tells the
function to use the buffer-local version of the hook."
  (remove-hook 'after-save-hook
               (if (stringp fname)
                   (intern fname)
                 fname)
               local))

;;;###autoload
(defun aftersave/remove-hook (fname &optional local)
  "Remove a registered function from the `after-save-hook'.

`FNAME' is the name of the function, and `LOCAL' tells the
function to use the buffer-local version of the hook.

If you use the Universal Argument (Ctrl-u), `LOCAL' will be
true."
  (interactive (list
                (let ((local (equal current-prefix-arg '(4))))
                  (ido-completing-read
                   "aWhich function: "
                   (aftersave/-get-hook-func-names local)))))
  (let ((local (or local
                   (equal current-prefix-arg '(4)))))
    (aftersave/-remove-hook fname local)))

;;;###autoload
(defun aftersave/add-hook (fname &optional local)
  "Add a function to the `after-save-hook' list.

`FNAME' is the name of the function, and `LOCAL' tells the
function to use the buffer-local version of the hook.

If you use the Universal Argument (Ctrl-u), `LOCAL' will be
true."
  (interactive "aWhich function: ")
  ;; check if either receive a local parameter _or_ ctrl-u was used
  (let ((local (or local
                   (equal current-prefix-arg '(4)))))
    (add-hook 'after-save-hook fname t local)))

(provide 'aftersave)
;;; aftersave.el ends here
