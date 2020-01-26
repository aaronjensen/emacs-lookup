(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.
PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:
- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.
This is a wrapper around `eval-after-load' that:
1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              (let ((body (macroexp-progn body)))
                `(if (featurep ',package)
                     ,body
                   ;; We intentionally avoid `with-eval-after-load' to prevent
                   ;; eager macro expansion from pulling (or failing to pull) in
                   ;; autoloaded macros/packages.
                   (eval-after-load ',package ',body)))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

;;;###autoload
(defun doom-region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

;;;###autoload
(defun doom-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.
Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.
NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (buffer-substring-no-properties
          (doom-region-beginning)
          (doom-region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; A little smarter than using `symbol-at-point', though in most cases,
         ;; xref ends up using `symbol-at-point' anyway.
         (xref-backend-identifier-at-point (xref-find-backend)))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defmacro use-package! (&rest ignore))
(defmacro featurep! (&rest ignore)
  nil)

(defun doom-log (&rest ignore))

(defun doom-set-jump-a (orig-fn &rest args)
  "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
  (better-jumper-set-jump (if (markerp (car args)) (car args)))
  (let ((evil--jumps-jumping t)
        (better-jumper--jumping t))
    (apply orig-fn args)))

(provide 'lookup-shim)
