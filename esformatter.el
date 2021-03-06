(defcustom esformatter-command "esformatter"
  "The esformatter command."
  :type 'string :group 'js)

(defun esfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; delete the current line without putting it in the kill-ring
;; copied from go-mode.el
(defun esfmt--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

;; copied from go-mode.el
(defun esfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in esfmt--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (esfmt--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (esfmt--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in esfmt--apply-rcs-patch")))))))))

(defun esformatter ()
  "Formats the current buffer with esformatter."
  (interactive)
  (let ((tmpfile   (make-temp-file "esformatter--" nil ".js"))
        (patchbuff (get-buffer-create "*esformatter patch*"))
        (errbuff   (get-buffer-create "*esformatter errors*"))
        (coding-system-for-read  'utf-8)
        (coding-system-for-write 'utf-8))

    (with-current-buffer errbuff
      (setq buffer-read-only nil)
      (erase-buffer))

    (with-current-buffer patchbuff
      (erase-buffer))

    ;; write current buffer to the tmpfile
    (write-region nil nil tmpfile)

    ;; overwrite tmpfile, and save contents to errbuff
    (call-process esformatter-command nil errbuff nil "-i" tmpfile)

    ;; take file being edited, diff it with errbuff, write result to patchbuff
    (call-process-region
                (point-min) (point-max)
                "diff" nil patchbuff nil "-n" "-" tmpfile)

    ;; apply diff to current buffer
    (esfmt--apply-rcs-patch patchbuff)

    ;; clean up
    (kill-buffer errbuff)
    (kill-buffer patchbuf)
    (delete-file tmpfile)))
