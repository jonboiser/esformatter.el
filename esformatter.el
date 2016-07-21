(defcustom esfmt-command "esformatter"
  "The esformatter command."
  :type 'string :group 'js)

(defun esfmt--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

;; delete the current line without putting it in the kill-ring
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

(defun esfmt--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
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


(defun run-esfmt ()
  "Formats the current buffer with esformatter."
  (interactive)
  (let ((tmpfile  (make-tempfile "esformatter--" nil ".js"))
        (patchbuf (get-buffer-create "*esformatter patch*"))
        (errbuf   (get-buffer-create "*esformatter errors*"))
        (coding-system-for-read  'utf-8)
        (coding-system-for-write 'utf-8)))

  (with-current-buffer errbuff
    (setq buffer-read-only nil)
    (erase-buffer))

  (with-current-buffer patchbuf
    (erase-buffer))

  ;; write current buffer to the tmpfile
  (write-region nil nil tmpfile)

  ;; happy path command
  ;; write formatted file to errbuf
  (call-process esformatter-command nil errbuf nil tmpfile)
  ;; write diff to patchbuf. argument -n means format as RCS diff
  (call-process-region
    (point-min) (point-max)
    "diff" nil patchbuf nil "-n" "-" tmpfile)
  ;; apply diff to current buffer
  (esfmt--apply-rcs-patch patchbuf)
  (kill-buffer patchbuf)
  (delete-file tmpfile))
