;;; code-shy.el -*- lexical-binding: t; no-byte-compile: nil; -*-
;;-- requires
(eval-when-compile
  (require 'vimish-fold)
  (require 's)
  (require 'evil)
  (require 'ivy)
  (require 'f)
  (require 'dash)

  (declare-function f-files "f")
  (declare-function s-trim "s")
  (declare-function s-concat "s")
  (declare-function s-join "s")
  (declare-function s-matches? "s")
  (declare-function s-contains? "s")
  (declare-function vimish-fold-delete-all "vimish-fold")
  (declare-function vimish-fold--folds-in "vimish-fold")
  (declare-function vimish-fold "vimish-fold")
  (declare-function evil-signal-at-bob-or-eob "evil")
  (declare-function -contains? "dash")
  (declare-function ivy-read "ivy")
  )
;;-- end requires

;;-- vars

(defvar-local code-shy-fold-patterns (list "%s -- %s %s" "%s -- %s %s") "Patterns to format with comment, end? and name form" )

(defvar-local code-shy-block-depth 2)

(defvar code-shy-start-hidden t)

(defvar code-shy-exclusions '(helm-major-mode ivy-mode minibuffer-mode dired-mode fundamental-mode))

(defvar code-shy-derivations '(prog-mode bibtex-mode conf-mode latex-mode))

(defvar-local code-shy--last-fold nil)

;;-- end vars

;;-- main functions
(cl-defun code-shy-fold-block-gen ( &rest rst &key
                                          (name "\\(.+\\)")
                                          (end nil)
                                          (re nil)
                                          (newlines nil)
                                          (comment comment-start)
                                          )
  " Single point to build fold block markers
Auto-recognizes the current major-mode's commment syntax

keys:
:name     - the name of the block to generate
:end      - generate the end block instead of start
:re       - generate a regex for searching for blocks
:newlines - add a newline to the generated block
:comment  - comment syntax override, defaults to major mode's `comment-start`

 "
  (let* ((comment-str (apply 'concat (make-list code-shy-block-depth (s-trim (or comment ";;")))))
         (end-str (when end "end "))
         (name-form (s-concat end-str name))
         (full-pattern
          (format "\\(?:%s\\)" (s-join "\\|" (mapcar #'(lambda (x) (s-trim (format x comment-str name-form comment-end))) code-shy-fold-patterns)))
          )
         (single-pattern (s-trim (format (car code-shy-fold-patterns) comment-str name-form comment-end)))
         )
    (cond ((and re newlines)         (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^.?*" full-pattern))
          (newlines (s-concat (if end "\n" "") single-pattern "\n"))
          (t full-pattern))))

(defun code-shy-run-folds ()
  " Add auto-hiding on buffer open.
Vimish-fold's any blocks matching code-shy-fold-patterns
"
  (interactive)
  ;; (message "Running Auto Hide: %s %s" major-mode comment-start)
  (save-excursion
    (goto-char (point-min))
    (vimish-fold-delete-all)
    ;; Fold Groups
    ;; (message "Searching for Fold Blocks")
    (let (group-name start-hide end-hide)
    (while (re-search-forward (code-shy-fold-block-gen :re t) nil t)
      (setq group-name (match-string 1)
            start-hide nil
            end-hide nil)
      (cond ((and (s-matches? "^end" group-name)
                  (not start-hide))
             (error "Found an End Block Too Early: %s" group-name))
            ((s-matches? "^end" group-name)
             nil)
            (t
             (setq start-hide (line-beginning-position)
                   end-hide (if (re-search-forward (code-shy-fold-block-gen :name group-name :end t :re t) nil t)
                                (line-end-position)
                              (error "Couldn't find: %s" group-name))))
            )

      (when (and start-hide end-hide (not (vimish-fold--folds-in start-hide end-hide)))
        ;; (message "Folding: %s %s %s" group-name start-hide end-hide)
        ;; (put-text-property start-hide end-hide 'cursor-sensor-functions (list #'code-shy-sensor-h))
        ;; (put-text-property start-hide end-hide 'front-sticky t)
        (vimish-fold start-hide end-hide)
        (goto-char end-hide))
      (forward-line)
      )
    )
    )
  )

(defun code-shy-clear-vimish-cache ()
  (mapc 'f-delete (f-files vimish-fold-dir))
  )

;;-- end main functions

;;-- mode definition

(define-minor-mode code-shy-minor-mode
  " Minor mode to automatically hide blocks of text upon loading a buffer "
  :init-value nil
  :lighter "CodeShy"
  :group 'code-shy
  (cond ((or (not code-shy-minor-mode)
             (minibufferp)
             (-contains? code-shy-exclusions major-mode)
             (not (apply 'derived-mode-p code-shy-derivations))
             )
         nil)
        ((and code-shy-minor-mode code-shy-start-hidden)
         (code-shy-run-folds)
         (if cursor-sensor-mode
             (code-shy--register-sensor)
           (add-hook 'cursor-sensor-mode-hook #'code-shy--register-sensor)
           )
         )
        (code-shy-minor-mode
         (code-shy-clear-vimish-cache)
         )
        )
  )

(define-globalized-minor-mode global-code-shy-minor-mode
  code-shy-minor-mode code-shy-minor-mode
  :group 'code-shy
  )

;;-- end mode definition

;;-- evil
(evil-define-operator code-shy-wrap-block (beg end _ &optional name)
  " Operator to easily create fold blocks "
  :type line
  :keep-visual t
  (interactive "<R>" (list (read-string "Block Name: ")))
  ;; From bottom to top to not change interfere with positions
  ;; add end fold block
  (goto-char end)
  (end-of-line)
  (insert (code-shy-fold-block-gen :name name :end t :newlines t))
  ;; and start fold block
  (goto-char beg)
  (beginning-of-line)
  (insert (code-shy-fold-block-gen :name name :newlines t))
  )

(evil-define-motion code-shy-forward-block (count)
  " Move forward across fold blocks "
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (re-search-forward (code-shy-fold-block-gen :re t) nil t count)
)

(evil-define-motion code-shy-backward-block (count)
  " Move forward across fold blocks "
  :jump t
  :type exclusive
  (code-shy-forward-block (- (or count 1)))
  )

;;-- end evil

;;-- ivy

(defun code-shy-fold-jump-to-heading ()
  " Ivy to jump to code-shy sections  "
  (interactive)
  (let (sections)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (code-shy-fold-block-gen :re t) nil t)
        (if (not (s-contains? "end " (match-string 1)))
            (push (cons (match-string 1) (point)) sections)
          )
        )
      )
    (ivy-read "Heading: " sections
              :action #'(lambda (x)
                          (goto-char (cdr x))
                          (beginning-of-line)
                          (vimish-fold-unfold)
                          )
              )
    )
  )

;;-- end ivy

;;-- sensor

(defun code-shy--register-sensor ()
  (add-hook 'post-command-hook #'code-shy-sensor-h 90 t))

(defun code-shy-sensor-h ()
  (interactive)
  ;; Cleanup/exit
  (cond ((not code-shy--last-fold)
         nil)
        ((not (overlayp code-shy--last-fold))
         (setq code-shy--last-fold nil))
        ((not (buffer-live-p (overlay-buffer code-shy--last-fold)))
         (delete-overlay code-shy--last-fold)
         (setq code-shy--last-fold nil)
         )
        ((not (and (<= (overlay-start code-shy--last-fold) (point))
                   (<= (point) (overlay-end code-shy--last-fold))))
         (save-excursion
           (vimish-fold--refold code-shy--last-fold)
           )
         (setq code-shy--last-fold nil)
         )
        )
  ;; enter
  (unless code-shy--last-fold
    (-when-let (ov (-filter #'vimish-fold--vimish-overlay-p (overlays-at (point))))
        (setq code-shy--last-fold (car ov))
        (vimish-fold--unfold (car ov))
        )
    )
  )

;;-- end sensor

(provide 'code-shy-minor-mode)
;;; code-shy-minor-mode.el ends here
