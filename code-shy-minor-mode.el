;;; code-shy.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- requires
(require 'vimish-fold)
(require 's)
(require 'evil)
(require 'ivy)
(require 'f)
;;-- end requires

;;-- vars
(defvar-local code-shy-fold-pattern "%s-- %s %s")
(defvar-local code-shy-block-depth 2)
(defvar code-shy-start-hidden t)
(defvar code-shy-exclusions '(helm-major-mode ivy-mode minibuffer-mode dired-mode fundamental-mode))
(defvar code-shy-derivations '(prog-mode bibtex-mode conf-mode latex-mode))
;;-- end vars

;;-- main functions
(cl-defun code-shy-fold-block-gen ( &rest rst &key (name "\\(.+\\)") (end nil) (re nil) (newlines nil) (comment comment-start))
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
         (full-pattern (s-trim (format code-shy-fold-pattern comment-str name-form comment-end)))
         )
    (cond ((and re newlines)         (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^[[:blank:]]*" full-pattern))
          (newlines (s-concat (if end "\n" "") full-pattern "\n"))
          (t full-pattern))))

(defun code-shy-run-folds ()
  " Add auto-hiding on buffer open.
Vimish-fold's any blocks matching code-shy-fold-pattern
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

;;-- end main functions

;;-- mode definition
(define-minor-mode code-shy-minor-mode
  " Minor mode to automatically hide blocks of text upon loading a buffer "
  :init-value nil
  :lighter "CodeShy"
  (cond ((or (not code-shy)
             (minibufferp)
             (-contains? code-shy-exclusions major-mode)
             (not (apply 'derived-mode-p code-shy-derivations))
             )
         nil)
        ((and code-shy code-shy-start-hidden)
         (code-shy-run-folds))
        (code-shy
         (code-shy-clear-vimish-cache)
         )
        )
  )

(define-globalized-minor-mode global-code-shy-minor-mode
  code-shy code-shy)

;;-- end mode definition

;;-- evil operator
(evil-define-operator code-shy-wrap-block (beg end type &optional name)
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


;;-- end evil operator

;;-- ivy
(defun code-shy-fold-jump-to-heading ()
  " Ivy to jump to code-shy sections  "
  (interactive)
  (let (sections)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (code-shy-fold-block-gen :re t) nil t)
        (if (not (s-contains? "end " (match-string 1)))
            (push (match-string 1) sections)
          )
        )
      )
    (goto-char (point-min))
    (re-search-forward (code-shy-fold-block-gen :name (ivy-read "Heading: " sections)) nil t)
    (beginning-of-line)
    )
  )

;;-- end ivy

(provide 'code-shy-minor-mode)
