;;; ack.el --- Ack frontend for Emacs
;; Copyright (C) 2015 Aurélien Aptel

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Aurélien Aptel <aurelien.aptel@gmail.com>
;; Version: 0.1
;; Package-Requires: ((shell-split-string "0.1"))
;; Keywords: grep, ack
;; URL:

;;; Commentary:

;; Define `ack' interactive function. I hate having to M-x cd before
;; doing searches and I hate having to remember what each buffer
;; current directory is. So ack uses a global search dir (not buffer
;; local).

;; TODO: don't hardcode colors
;; TODO: define major mode with usual kbinding (eg. g)
;; TODO: define faces for match, line numbers, file paths
;; TODO: make file paths clickable

(require 'shell-split-string)
(require 'button)

;;(setq ack-file-rx (rx bol "\033[" (repeat 2 6 (any "0-9;")) "m" (group (+? any)) "\033[0m"))
;;(setq ack-line-rx (rx bol "\033[" (repeat 2 6 (any "0-9;")) "m" (group (+ digit)) "\033[0m"))
;;(setq ack-match-rx (rx "\033[" (repeat 2 6 (any "0-9;")) "m" (group (+ digit)) "\033[0m"))

(defvar ack-file-rx (rx bol "\033[1;32m" (group (+? any)) "\033[0m"))
(defvar ack-line-rx (rx bol "\033[1;33m" (group (+ digit)) "\033[0m" (? "\033[K") (any ":-")))
(defvar ack-match-rx (rx "\033[30;43m" (group (+? any)) "\033[0m"))
(defvar ack-garbage-rx (rx (? "\033[0m") "\033[K"))
(defvar ack-process nil)
(defvar ack-dir nil)
(defvar ack-base-cmd "-C 1 ")
(defvar ack-cmd ack-base-cmd)
(defvar ack-buffer nil)
(defcustom ack-prog "ag"
  "Ack program")

(defface ack-file-face
  '((t . (:foreground "orange")))
  "Face used for files.")

(defface ack-line-face
  '((t . (:foreground "green")))
  "Face used for line numbers.")

(defface ack-match-face
  '((t . (:foreground "red")))
  "Face used for matching text.")


(define-derived-mode ack-mode special-mode "Ack"
  "Major mode for viewing Ack results.
\\{ack-mode-map}"
  (define-key ack-mode-map (kbd "RET") 'ack-goto-match)
  (define-key ack-mode-map (kbd "TAB") 'ack-next-match))

;;;###autoload
(defun ack (arg)
  (interactive "P")
  (when (or arg (not ack-dir))
    (setq ack-dir (read-directory-name "Ack directory: ")))
  (when (not (string-match-p (rx "/" eos) ack-dir))
    (setq ack-dir (concat ack-dir "/")))

  (let ((cmd (concat ack-base-cmd (thing-at-point 'symbol))))
    (setq ack-cmd (read-string "Ack cmd: " cmd)))

  (let ((buf (get-buffer-create "*Ack*")))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((default-directory ack-dir)
	    (buffer-read-only nil))
	(erase-buffer)
	(insert (format "  [%s] %s\n\n" ack-dir ack-cmd))
	(setq ack-process (apply 'start-process "ack" buf ack-prog (shell-split-string ack-cmd)))
	(set-process-filter ack-process 'ack-process-filter)
	(set-process-sentinel ack-process 'ack-process-sentinel)
	(ack-mode))
      )
    (setq ack-buffer buf)
    (ack-show-ack-results buf)))

(defun ack-goto-match ()
  (interactive)
  (let ((fn (ack-match-file))
	(ln (ack-match-line)))
    (with-current-buffer (find-file-other-window fn)
      (goto-line ln))))

(defun ack-next-match ()
  (interactive)
  (catch :exit
    (let ((p (point)))
      (while p
	(setq p (next-property-change p))
	(when (and p (eq (get-text-property p 'face) 'ack-match-face))
	  (goto-char p)
	  (throw :exit nil))))))


(defun ack-prev-match ()
  (interactive)
  (catch :exit
    (let ((p (point)))
      (while p
	(setq p (previous-property-change p))
	(when (and p (eq (get-text-property p 'face) 'ack-match-face))
	  (goto-char p)
	  (throw :exit nil))))))


(defun ack-match-line ()
  "Return the line of the current match."
  (save-excursion
    (beginning-of-line)
    (when (looking-at (rx (group (+ digit))))
      (string-to-number (match-string 1)))))

(defun ack-find-first (pred sequence)
  (catch :exit
    (mapc (lambda (x) (when (funcall pred x) (throw :exit x))) sequence)
    nil))

(defun ack-match-file ()
  "Return the file of the current line match."
  (save-excursion
    (beginning-of-line)
    (let ((continue t))
      (while continue
	(goto-char (previous-property-change (point)))
	(backward-char)	(backward-char) ;; wat
	(let* ((face (plist-get (text-properties-at (point)) 'face)))
	  (when (eq face 'ack-file-face)
	    (setq continue nil))))
      (beginning-of-line)
      ;; lets try everything...
      (let* ((file1 (buffer-substring (point) (save-excursion (end-of-line) (point))))
	     (file2 (concat default-directory file1))
	     (file3 (concat ack-dir file1)))
	(ack-find-first 'file-exists-p (list file3 file2 file1))))))

(defun ack-line-goto-match (btn)
  (save-excursion
    (goto-char (button-start btn))
    (let ((file (ack-match-file))
	  (line (ack-match-line)))
    (with-current-buffer (find-file-other-window file)
      (goto-line line)))))

(defun ack-filter-region (beg end)
  (save-excursion
    (cl-assert (markerp end))
    (let ((endp (marker-position end)))
      (goto-char beg)
      (while (re-search-forward ack-file-rx endp 1)
	(replace-match (propertize (match-string 1) 'face 'ack-file-face) t t))

      (goto-char beg)
      (while (re-search-forward ack-line-rx endp 1)
	(let ((s (concat (match-string 1) " ")))
	  (replace-match "")
	  (insert-text-button s 'face 'ack-line-face 'action 'ack-line-goto-match)
	  (incf endp)))

      (goto-char beg)
      (while (re-search-forward ack-match-rx endp 1)
	(replace-match (propertize (match-string 1) 'face 'ack-match-face) t t))

      (goto-char beg)
      (while (re-search-forward ack-garbage-rx endp 1)
	(replace-match "" t t)))))

(defun ack-process-sentinel (proc event)
  (when (and (string-match-p (rx bos (or "finished" "exited")) event) (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((buffer-read-only nil))
	(ack-filter-region (point-min) (point-max-marker))
	(save-excursion
	  (goto-char (point-max))
	  (insert "\nAck finished!\n"))))))

(defun ack-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((buffer-read-only nil)
	    (moving (= (point) (process-mark proc)))
	    start
	    end)

        (save-excursion
	  (let ((buffer-read-only nil))
	    (goto-char (process-mark proc))
	    (setq start (point))
	    (insert string)
	    (setq end (point-marker))
	    (ack-filter-region start end)
	    (set-marker (process-mark proc) (point-max))))

        (if moving (goto-char (process-mark proc)))))))


;; C stuff for samba..
(defun ack-find-thing (sym pattern)
  (let* ((cmd (format "( cd '%s' ; ack --cc '%s' -m1 --no-group --no-color)" (expand-file-name ack-dir) pattern))
	 (res (shell-command-to-string cmd)))
    ;; (message "CMD<%s>" cmd)
    ;; (message "RES<%s>" res)
    (if (string-match (rx bos (group (+? any)) ":" (group (+ digit)) ":") res)
	(let ((file (match-string 1 res))
	      (ln (string-to-int (match-string 2 res))))
	  (find-file (concat ack-dir file))
	  (goto-line ln))
      (error "can't find %s" sym))))

(defun ack-find-function-definition (fun)
  (interactive
   (list (read-string "function? " (thing-at-point 'symbol))))
  (ack-find-thing fun (format "^[^\\t]*\\b%s\\s*\\([^;]+?[,\\)]\\s*?[^;]$" fun)))

(defun ack-find-struct-definition (struct)
  (interactive
   (list (read-string "struct? " (thing-at-point 'symbol))))
  (ack-find-thing struct (format "^struct %s\\s*\\{" struct)))

(defun ack-find-macro (macro)
  (interactive
   (list (read-string "macro? " (thing-at-point 'symbol))))
  (ack-find-thing macro (format "^\\s*#define\\s+%s\\b" macro)))

;; (defun ack-find-local-var-definition (var)
;;   (interactive
;;    (list (read-string "var? " (thing-at-point 'symbol))))
;;   (search-backward-regexp (eval `(rx ,var ";"))))

;; poor man's tags navigation stack

(defvar ack-jmp-stack nil)

(defun ack-jmp-push ()
  (interactive)
  (push (cons (current-buffer) (point)) ack-jmp-stack)
  (ack-find-function-definition (thing-at-point 'symbol)))

(defun ack-jmp-top ()
  (interactive)
  (let ((top (car ack-jmp-stack)))
    (when top
      (switch-to-buffer (car top))
      (goto-char (cdr top)))))

(defun ack-jmp-pop ()
  (interactive)
  (ack-jmp-top)
  (pop ack-jmp-stack))

(defun ack-jmp-reset ()
  (interactive)
  (setq ack-jmp-stack nil))

(defun ack-show-ack-results (buf)
  (let ((win
	 (catch :win
	   (dolist (w (window-list))
	     (when (eq (window-buffer w) buf)
	       (throw :win w))))))
    (if win
	(set-window-buffer win buf)
      (switch-to-buffer-other-window buf))))

(provide 'ack)
;;; ack.el ends here
