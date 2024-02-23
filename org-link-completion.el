;;; org-link-completion.el ---  Org Link Completion At Point  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: outlines

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'ol)


;;;; Setup

;;;###autoload
(defun org-link-completion-setup ()
  (interactive)
  (with-eval-after-load "org"
    (org-link-completion-setup-type-file)
    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions
                          'org-link-completion-at-point nil t)))))


;;;; Parse Link At Point

;; [[c:/home/
;; [[#my-custom-id]
;; [[My Target]
;; [[/dir/file]
;; [[./dir/file]
;; [[*Heading]
;; [[file:/dir/file::My Target]
;; [[file:test.org
;; [[type:<path>]
;; [[    ][
;; [[    ][  ]]
;; [[    ][  ][   ][   ]]
;; [[type:path][foofoofoo[[foofofofof[[fofof]] <= Unable to parse correctly

(defconst org-link-completion-type-chars "-A-Za-z0-9_+")

(defun org-link-completion-parse-at-point ()
  "Return a list in the following format:
(WHERE TYPE-BEG TYPE-END [ PATH-BEG PATH-END [ DESC-BEG DESC-END ] ])"
  (save-excursion
    (let ((origin (point))
          type-beg type-end type-sep
          path-beg path-end
          desc-beg desc-end)
      ;; Search back [[ and record location of ][
      (while (progn
               (skip-chars-backward "^\n][")
               (pcase (char-before)
                 (?\[ (pcase (char-before (1- (point)))
                        ;; ][ => record & skip
                        (?\] (setq desc-beg (point)) (backward-char 2) t)
                        ;; [[ => stop
                        (?\[ (setq type-beg (point)) nil)
                        ;; ?[ => skip
                        (_   (backward-char) t)))
                 (?\] (if (or (eq (char-before (1- (point))) ?\])
                              (eq (char-after (point)) ?\]))
                          ;; ]] => stop
                          nil
                        ;; ?] => skip
                        (backward-char)
                        t))
                 ;; \n or nil => stop
                 (_   nil))))
      (when type-beg ;; [[ not \] \n nil
        (setq type-end (progn
                         (skip-chars-forward org-link-completion-type-chars)
                         (point))
              type-sep (eq (char-after) ?:)
              path-end (progn
                         ;; Skip escape sequence \\ \] \[ or not \ ] [ \n
                         (while (progn
                                  (skip-chars-forward "^\n][\\\\")
                                  (when (eq (char-after) ?\\)
                                    (forward-char)
                                    (when (memq (char-after)
                                                '(?\\ ?\[ ?\]))
                                      (forward-char))
                                    t)))
                         (point)))

        ;; c in c:/ is not a type
        (when (and type-sep
                   (= (- type-end type-beg) 1)
                   (let ((ch (char-after type-beg)))
                     (or (<= ?a ch ?z) (<= ?A ch ?Z))))
          (setq type-end type-beg
                type-sep nil))

        ;; `::' is not a type separator
        (when (and type-sep
                   (eq (char-after (1+ type-end)) ?:))
          (setq type-end type-beg
                type-sep nil))

        ;; No type separator
        (when (and (< type-end origin)
                   (not type-sep))
          (setq type-end type-beg))

        ;; Beginning of path part
        (setq path-beg (+ type-end (if type-sep 1 0)))

        (cond
         ;; [[<type>
         ((<= origin type-end) (list 'type type-beg type-end))
         ;; [[<type>:<path>
         ;; [[#customid
         ;; [[*heading
         ;; [[/home/test.org
         ;; [[My Target
         ((<= origin path-end) (list 'path type-beg type-end path-beg path-end))
         ;; [[<type>:<path>]
         ((null desc-beg) nil)
         ;; Reject invalid [[path\][desc
         ((/= (- desc-beg 2) path-end) nil)
         ;; [[<type>:<path>][
         (t
          ;; Find ]] or \n
          (goto-char desc-beg)
          (setq desc-end
                (progn
                  (while (progn
                           (skip-chars-forward "^]\n")
                           (when (and (eq (char-after) ?\])
                                      (not (eq (char-after (1+ (point))) ?\])))
                             (forward-char)
                             t)))
                  (point)))
          ;; [[<type>:<path>][<description>
          (when (<= origin desc-end)
            (list 'desc type-beg type-end path-beg path-end desc-beg desc-end)
            )))))))

(defvar org-link-completion-pos nil
  "Temporarily hold result of `org-link-completion-parse-at-point'
function.")

(defmacro org-link-completion-pos-ref (pos pname &optional where)
  "Return PNAME property of parsing result POS at WHERE.

POS is a return value of `org-link-completion-parse-at-point'.

PNAME is one of the following:

- where
- type-beg
- type-end
- path-beg
- path-end
- desc-beg
- desc-end
- type
- path
- desc

Example: (org-link-completion-pos-ref pos path-beg)"
  (let* ((path-p (memq where '(nil :desc :path)))
         (desc-p (memq where '(nil :desc)))
         (pos-sym (if (symbolp pos) pos (gensym "pos-")))
         (pos-refs nil)
         (expr
          (pcase pname
            ('where    (setq pos-refs 1) `(nth 0 ,pos-sym))
            ('type-beg (setq pos-refs 1) `(nth 1 ,pos-sym))
            ('type-end (setq pos-refs 1) `(nth 2 ,pos-sym))
            ('path-beg (setq pos-refs 1) (and path-p `(nth 3 ,pos-sym)))
            ('path-end (setq pos-refs 1) (and path-p `(nth 4 ,pos-sym)))
            ('desc-beg (setq pos-refs 1) (and desc-p `(nth 5 ,pos-sym)))
            ('desc-end (setq pos-refs 1) (and desc-p `(nth 6 ,pos-sym)))
            ('type `(buffer-substring-no-properties (nth 1 ,pos-sym)
                                                    (nth 2 ,pos-sym)))
            ('path (and
                    path-p
                    `(buffer-substring-no-properties (nth 3 ,pos-sym)
                                                     (nth 4 ,pos-sym))))
            ('desc (and
                    desc-p
                    `(buffer-substring-no-properties (nth 5 ,pos-sym)
                                                     (nth 6 ,pos-sym)))))))
    (unless expr
      (error "Unknown property `%s' for org-link-completion-parsed" pname))
    (if (or (eq pos-sym pos) (eq pos-refs 1))
        expr
      `(let ((,pos-sym ,pos)) ,expr))))

(defmacro org-link-completion-parse-let (where prop-names &rest body)
  "Parse link text and bind variables to results.

Assuming that point is at the WHERE position of the link, parse
the surrounding area, extract the property specified by
PROP-NAMES from the result, and evaluate BODY.

WHERE is the expected point location and can be specified as nil,
:type, :path, or :desc. If the point is not in the specified part
of the link, do nothing. nil means to accept any part."
  (declare (indent 2))
  (let* ((pos-sym (gensym "pos"))
         (pos-type (pcase where
                     (:type 'type)
                     (:path 'path)
                     (:desc 'desc)
                     ('nil nil)
                     (_ (error "Invalid where=%s" where))))
         (bindings (cl-loop for pname in prop-names
                            collect
                            `(,pname
                              (org-link-completion-pos-ref
                               ,pos-sym ,pname ,where)))))

    `(let ((,pos-sym (or org-link-completion-pos
                         (org-link-completion-parse-at-point))))
       ,(if pos-type
            `(when (eq (car-safe ,pos-sym) ',pos-type)
               (let ,bindings
                 ,@body))
          `(let ,bindings
             ,@body)))))


;;;; Completion At Point Function

(defvar org-link-completion-capf-type
  #'org-link-completion-capf-type-default)

(defvar org-link-completion-capf-path-untyped
  #'org-link-completion-capf-path-untyped-default)

(defvar org-link-completion-capf-desc-untyped
  #'org-link-completion-capf-desc-untyped-default)

;;;###autoload
(defun org-link-completion-at-point ()
  "Complete the path or description part of a link on point.

If point is on a link (including unfinished links), the
appropriate function is called depending on the part and link
type.

[[<type>:<path>][<desc>]]

- point is on <type> => call `org-link-completion-capf-type' variable

- <type> is empty:

  - point is on <path> => call `org-link-completion-capf-path-untyped' variable
  - point is on <desc> => call `org-link-completion-capf-desc-untyped' variable

- <type> is a valid link type:

  call one of functions set to the following properties of
  `org-link-parameters'.

  - `:capf-path' : point is on <path>
  - `:capf-desc' : point is on <desc>
  - `:completino-at-point' : If the above properties are not present.

No arguments are passed to the function. However, before it is
called, the variable `org-link-completion-pos' is set with
information about the link to be completed.

The function must return the same format as functions added to
`completion-at-point-functions'.

To use this, do the following in org-mode buffer:
(add-hook \\='completion-at-point-functions
          #\\='org-link-completion-at-point nil t)"
  (when-let ((pos (org-link-completion-parse-at-point)))
    (let* ((org-link-completion-pos pos)
           (where (org-link-completion-pos-ref pos where))
           (type (org-link-completion-pos-ref pos type)))
      (cond
       ((eq where 'type)
        (org-link-completion-call org-link-completion-capf-type))
       ((string-empty-p type)
        (pcase where
          ('path (org-link-completion-call
                  org-link-completion-capf-path-untyped))
          ('desc (org-link-completion-call
                  org-link-completion-capf-desc-untyped))))
       (t
        (let* ((capf-prop (if (eq where 'desc) :capf-desc :capf-path))
               (capf (or (org-link-get-parameter type capf-prop)
                         (org-link-get-parameter type :completion-at-point))))
          ;;(message "capf=%s" capf)
          (when capf
            (funcall capf))))))))

(defun org-link-completion-call (fun)
  (when (functionp fun)
    (funcall fun)))


;;;; Complete Link Type Part

(defun org-link-completion-capf-type-default ()
  ;; <type>:
  ;; TODO: If <type> is a target link.
  (org-link-completion-parse-let :type (type-beg type-end)
    (list
     type-beg
     (if (eq (char-after type-end) ?:) (1+ type-end) type-end)
     (mapcar (lambda (e) (concat (car e) ":"))
             (append org-link-abbrev-alist-local
                     org-link-abbrev-alist
                     org-link-parameters)))))

;;;; Complete Untyped Link

(defun org-link-completion-capf-path-untyped-default ()
  ;; #<custom id>
  ;; *<heading>
  ;; <target>
  ;; <file>
  )

(defun org-link-completion-capf-desc-untyped-default ()
  ;; path is:
  ;; #<custom id>
  ;; *<heading>
  ;; <target>
  ;; <file>
  )


;;;; Complete File Type Link

;;;###autoload
(defun org-link-completion-setup-type-file ()
  (dolist (type '("file" "file+sys" "file+emacs"))
    (org-link-set-parameters
     type
     :capf-path 'org-link-completion-capf-path-file
     :capf-desc 'org-link-completion-capf-desc-file)))

;;;###autoload
(defun org-link-completion-capf-path-file ()
  "Complete <filename> of [[<type>:<filename> at point.

This function also works for `file+sys:' and `file+emacs:' link types.

To enable this, call `org-lnk-completion-setup-type-file' function."
  (org-link-completion-parse-let :path (path-beg path-end)
    (list
     path-beg path-end
     #'read-file-name-internal
     :annotation-function
     (lambda (str) (if (string-suffix-p "/" str) " Dir" " File"))
     :company-kind
     (lambda (str) (if (string-suffix-p "/" str) 'folder 'file))
     :exclusive 'no)))

;;;###autoload
(defun org-link-completion-capf-desc-file ()
  "Complete <filename> of [[<type>:<filename>][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end path desc)
    (when (string-prefix-p desc path)
      (list
       desc-beg desc-end
       (list path)
       :company-kind (lambda (_) 'file)))))


(provide 'org-link-completion)
;;; org-link-completion.el ends here
