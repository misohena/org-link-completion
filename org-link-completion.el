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
(require 'org)
(require 'org-element)


(defgroup org-link-completion nil
  "Org link completion at point."
  :tag "Org Link Completion At Point"
  :prefix "org-link-completion-"
  :group 'org)

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

;; [[My Target]
;; [[*heading]
;; [[#my-custom-id]
;; [[file:test.org
;; [[file:/dir/file]
;; [[file:README.org::library]
;; [[file:README.org::*Setup]
;; [[file:README.org::#custom-id]
;; [[/dir/file]
;; [[./dir/file]
;; [[~/.emacs.d/]
;; [[c:/home/
;; [[type:<path>]
;; [[\[<type>:<path>\]][escape sequence
;; [[^#\\\\+TITLE][escape sequence
;; [[My Target][description[multiple[bracket]bracket]bracket]]
;; [[(jump)   (See: https://orgmode.org/manual/Literal-Examples.html)
;; [[(jump)][Line (jump)
;; [[    ][
;; [[    ][  ]]
;; [[    ][  ][   ][   ]]
;; [[type:path][foofoofoo[[foofofofof[[fofof]] <= Unable to parse correctly
;; [[My
;; Target]] <= Not supported
;;
;; Invalid Syntax:
;; [[My Target\][description]]
;; [[My [Target][description]]
;; [[My ]Target][description]]


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
         ;; Reject invalid syntax [[path\][desc , [[pa[th][desc, [[pa]th][desc
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

(defcustom org-link-completion-capf-type
  #'org-link-completion-capf-type-default
  "Function to complete link type part."
  :group 'org-link-completion
  :type 'function)

(defcustom org-link-completion-capf-path-untyped
  #'org-link-completion-capf-path-untyped-default
  "Function to complete path of a link that does not have type specification."
  :group 'org-link-completion
  :type 'function)

(defcustom org-link-completion-capf-desc-untyped
  #'org-link-completion-capf-desc-untyped-default
  "Function to complete description of a link that does not have
 type specification."
  :group 'org-link-completion
  :type 'function)

(defcustom org-link-completion-capf-path-unknown-type
  #'org-link-completion-capf-path-unknown-type-default
  "Function to complete path of a link that is unknown type."
  :group 'org-link-completion
  :type 'function)

(defcustom org-link-completion-capf-desc-unknown-type
  #'org-link-completion-capf-desc-unknown-type-default
  "Function to complete description of a link that is unknown type."
  :group 'org-link-completion
  :type 'function)

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
  - `:completion-at-point' : If the above properties are not present.

- No completion function found for <type>:

  - point is on <path> => call `org-link-completion-capf-path-unknown-type' var
  - point is on <desc> => call `org-link-completion-capf-desc-unknown-type' var

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
        (let* ((capf-prop (pcase where
                            ('path :capf-path)
                            ('desc :capf-desc)))
               (capf (or (org-link-get-parameter type capf-prop)
                         (org-link-get-parameter type :completion-at-point)
                         (pcase where
                           ('path org-link-completion-capf-path-unknown-type)
                           ('desc org-link-completion-capf-desc-unknown-type)
                           ))))
          ;;(message "capf=%s" capf)
          (when (functionp capf)
            (funcall capf))))))))

(defun org-link-completion-call (fun)
  (when (functionp fun)
    (funcall fun)))


;;;; Complete Link Unknown Type

(defun org-link-completion-capf-path-unknown-type-default ()
  (org-link-completion-capf-path-from-other-links))

(defun org-link-completion-capf-desc-unknown-type-default ()
  (org-link-completion-capf-desc-from-other-links))


;;;; Complete Link Type Part

(defcustom org-link-completion-capf-type-default-collectors
  '(org-link-completion-collect-type-part-internal-link-symbols
    org-link-completion-collect-types
    ;; Consider the possibility that <type> is <target>.
    ;; For example: [[mytarget .
    org-link-completion-collect-search-target)
  "List of functions that collect completion candidates for first
part of link."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-type-default ()
  "Comlete [[#<type>: part of link at point."
  (org-link-completion-parse-let :type (type-beg type-end)
    (org-link-completion-capf-result
     type-beg
     (if (eq (char-after type-end) ?:) (1+ type-end) type-end)
     (org-link-completion-call-collectors
      org-link-completion-capf-type-default-collectors)
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-type-part-internal-link-symbols ()
  (list
   (org-link-completion-annotate "#" "CUSTOM_ID")
   (org-link-completion-annotate "*" "Heading")
   (org-link-completion-annotate "(" "Coderef")))

(defun org-link-completion-collect-types ()
  (mapcar (lambda (e) (concat (car e) ":"))
          (append org-link-abbrev-alist-local
                  org-link-abbrev-alist
                  org-link-parameters)))


;;;; Complete Untyped Link

;; Complete links without <type>: specification.

;; `org-link-completion-capf-path-untyped-default'
;;
;; Internal Links
;;  [[#custom-id       => `org-link-completion-capf-path-custom-id'
;;   ][<description>]] => `org-link-completion-capf-desc-custom-id'
;;  [[*heading         => `org-link-completion-capf-path-heading'
;;   ][<description>]] => `org-link-completion-capf-desc-heading'
;;  [[(coderef)        => `org-link-completion-capf-path-coderef'
;;   ][<description>]] => `org-link-completion-capf-desc-coderef'
;;  [[My Target"       => `org-link-completion-capf-path-search'
;;   ][<description>]] => `org-link-completion-capf-desc-search'
;;
;; External Links
;;  [[./file           => `org-link-completion-capf-path-file' (same as file:)
;;   ][<description>]] => `org-link-completion-capf-desc-file' (same as file:)

(defun org-link-completion-untyped-link-kind (beg end)
  (when (< beg end)
    (pcase (char-after beg)
      ;; #<custom id>
      (?# 'custom-id)
      ;; *<heading>
      (?* 'heading)
      ;; (<coderef>)
      (?\( 'coderef)
      ;; <file>
      ((or ?/ ?. ?~
           ;; Drive letter (c:)
           (pred (lambda (ch)
                   (and (or (<= ?a ch ?z) (<= ?A ch ?Z))
                        (eq (char-after (1+ beg)) ?:)))))
       'file)
      ;; <target>
      (_
       'search))))

;;;;; Untyped Path

(defcustom org-link-completion-capf-path-untyped-functions
  '((custom-id . org-link-completion-capf-path-custom-id)
    (heading . org-link-completion-capf-path-heading)
    (coderef . org-link-completion-capf-path-coderef)
    (search . org-link-completion-capf-path-search)
    (file . org-link-completion-capf-path-file))
  "Alist of functions to complete path for each kind of untyped link."
  :group 'org-link-completion
  :type 'alist)

(defun org-link-completion-capf-path-untyped-default ()
  "Complete <path> of link that does not have <type>: at point.

For example:
  [[#custom-id
  [[*heading
  [[(coderef)
  [[My Target
  [[./file"
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-path-call-on-kind
     path-beg path-end
     org-link-completion-capf-path-untyped-functions)))

(defun org-link-completion-capf-path-call-on-kind (path-beg path-end alist)
  (let ((fun (alist-get
              (org-link-completion-untyped-link-kind path-beg path-end)
              alist)))
    (when (functionp fun)
      (funcall fun))))

;;;;;; Custom-ID Path

(defcustom org-link-completion-capf-path-custom-id-collectors
  '(org-link-completion-collect-custom-id)
  "List of functions that collect path completion candidates in
CUSTOM-ID format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-path-custom-id ()
  "Comlete [[#<custom-id> part of link at point."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     (1+ path-beg) path-end
     (org-link-completion-call-collectors
      org-link-completion-capf-path-custom-id-collectors)
     :kind 'keyword
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-custom-id ()
  "Collect all :CUSTOM_ID: property values from the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; See: `org-find-property'
    (let ((case-fold-search t)
          (re (org-re-property "CUSTOM_ID" nil nil nil)))
      (cl-loop while (re-search-forward re nil t)
               for id = (org-entry-get (point) "CUSTOM_ID" nil t)
               when id
               collect id))))

;;;;;; Heading Path

(defcustom org-link-completion-capf-path-heading-collectors
  '(org-link-completion-collect-heading)
  "List of functions that collect path completion candidates in
heading format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-path-heading ()
  "Comlete [[*<heading> part of link at point."
  ;; NOTE: There is already an implementation in
  ;; `pcomplete/org-mode/searchhead'
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     (1+ path-beg) path-end
     (org-link-completion-call-collectors
      org-link-completion-capf-path-heading-collectors)
     :kind 'folder
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-heading ()
  "Collect all heading text from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-outline-regexp nil t)
             for heading = (ignore-errors
                              (org-link--normalize-string
                               (substring-no-properties
                                ;; or nil
                                (org-get-heading t t t t))))
             when heading
             collect heading)))

;;;;;; Coderef Path

(defcustom org-link-completion-capf-path-coderef-collectors
  '(org-link-completion-collect-coderef)
  "List of functions that collect path completion candidates in
coderef format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-path-coderef ()
  "Comlete [[(<coderef>) part of link at point."
  ;; NOTE: There is already an implementation in
  ;; `pcomplete/org-mode/searchhead'
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     (1+ path-beg) path-end
     (org-link-completion-call-collectors
      org-link-completion-capf-path-coderef-collectors)
     :kind 'reference
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-coderef ()
  "Collect all coderef labels (ref:<label>) from the current buffer."
  ;; Ref: `org-link-search'
  (save-excursion
    (goto-char (point-min))
    (let (result)
      ;; For each code block
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (goto-char (match-beginning 0))
        (let ((block-beg (match-beginning 0))
              (block-end (match-end 0))
              (element (org-element-at-point))) ;;NOTE: Change match-data
          (when (and (memq (org-element-type element)
                           '(src-block example-block))
                     (<= block-beg
                         (org-element-property :post-affiliated element)))
            ;; For each coderef
            (let ((re-coderef (concat ".*?"
                                      (org-src-coderef-regexp
                                       (org-src-coderef-format element)))))
              (while (re-search-forward re-coderef block-end t)
                (push (match-string-no-properties 3) result))))
          (goto-char block-end)))
      (nreverse result))))


;;;;;; Search Target Path

(defcustom org-link-completion-capf-path-search-collectors
  ;; Ignore headings, all words.
  '(org-link-completion-collect-dedicated-target
    org-link-complete-collect-element-names)
  "List of functions that collect path completion candidates in
search target format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-path-search ()
  "Comlete `[[My Target' part of link at point.

NOTE: `[[mytarget' is treated as a link type named `mytarget:'."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-collect-search-target)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-search-target ()
  "Collect all search target strings from the current buffer."
  (org-link-completion-call-collectors
   org-link-completion-capf-path-search-collectors))

(defun org-link-completion-collect-dedicated-target ()
  "Collect all dedicated target (<<target>>) from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-target-regexp nil t)
             for target = (match-string 1)
             when target
             collect target)))

(defun org-link-complete-collect-element-names ()
  "Collect all element names (#+NAME:) from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^[ \t]*#\\+NAME:[ \t]*\\(.*?\\)[ \t]*$"
                                      nil t)
             collect (match-string-no-properties 1))))

;;;;; Untyped Description

(defcustom org-link-completion-capf-desc-untyped-functions
  '((custom-id . org-link-completion-capf-desc-custom-id)
    (heading . org-link-completion-capf-desc-heading)
    (coderef . org-link-completion-capf-desc-coderef)
    (search . org-link-completion-capf-desc-search)
    (file . org-link-completion-capf-desc-file))
  "Alist of functions to complete description for each kind of untyped link."
  :group 'org-link-completion
  :type 'alist)

(defun org-link-completion-capf-desc-untyped-default ()
  (org-link-completion-parse-let :desc (path-beg path-end)
    (org-link-completion-capf-path-call-on-kind
     path-beg path-end
     org-link-completion-capf-desc-untyped-functions)))

;;;;;; Custom-ID Description

(defcustom org-link-completion-capf-desc-custom-id-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-custom-id-desc-from-around-target
    org-link-completion-collect-stripped-internal-link-path)
  "List of functions that collect description completion candidates
in custom-id format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-desc-custom-id ()
  "Complete [[#custom-id][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-capf-desc-custom-id-collectors)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-custom-id-desc-from-around-target ()
  (org-link-completion-parse-let nil (path)
    ;; Extract from target location
    (save-excursion
      (delq nil
            (when (org-link-completion-link-search path)
              (list
               (org-link-completion-get-heading)))))))

;;;;;; Heading Description

(defcustom org-link-completion-capf-desc-heading-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-stripped-internal-link-path)
  "List of functions that collect description completion candidates
in heading format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-desc-heading ()
  "Complete [[*heading][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-capf-desc-heading-collectors)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

;;;;;; Search Target Description

(defcustom org-link-completion-capf-desc-search-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-search-desc-from-around-target
    org-link-completion-collect-stripped-internal-link-path)
  "List of functions that collect description completion candidates
in search target format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-desc-search ()
  "Complete [[My Target][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-capf-desc-search-collectors)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-search-desc-from-around-target ()
  (org-link-completion-parse-let nil (path)
    ;; Extract from target location
    (save-excursion
      (let ((table))
        (when (org-link-completion-link-search path)
          ;; TODO: Allow customization
          ;; Heading
          (push (org-link-completion-get-heading) table) ;; or nil

          ;; Current line text
          (push (org-link-completion-escape-description-string
                 (string-trim
                  ;; Remove <<...>> not <<<...>>>.
                  (replace-regexp-in-string
                   (concat "\\(?:[^<]\\|^\\)\\("
                           org-target-regexp
                           "\\)\\(?:[^>]\\|$\\)")
                   ""
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))
                   nil nil 1)
                  ;; Strip bullets and table separators
                  "\\(?:[ \t\n\r]*[|-]\\)*[ \t]*"
                  "\\(?:[ \t]*|\\)*[ \t]*"))
                table))
        (delq nil table)))))


;;;;;; Coderef Description

(defcustom org-link-completion-capf-desc-coderef-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-default-coderef-description
    org-link-completion-collect-path
    org-link-completion-collect-coderef-desc-from-around-target)
  "List of functions that collect description completion candidates
in coderef format."
  :group 'org-link-completion
  :type '(repeat (function)))

(defun org-link-completion-capf-desc-coderef ()
  "Complete [[(coderef)][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-capf-desc-coderef-collectors)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-coderef-desc-from-around-target ()
  (org-link-completion-parse-let nil (path)
    ;; Extract from target location
    (save-excursion
      (let (table)
        (when (org-link-completion-link-search path)
          ;; TODO: Allow customization
          ;; Heading
          (push (org-link-completion-get-heading) table);; or nil

          ;; Current line text
          (push (org-link-completion-escape-description-string
                 (string-trim
                  ;; Remove coderef target from the line.
                  (replace-regexp-in-string
                   (org-src-coderef-regexp (org-src-coderef-format
                                            (org-element-at-point)))
                   ""
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))))
                table))
        (delq nil table)))))

(defun org-link-completion-collect-default-coderef-description ()
  "Line (<coderef>)"
  (org-link-completion-parse-let nil (path) ;; "(<coderef>)" format
    (list (org-link-completion-default-coderef-description path))))

(defconst org-link-completion-default-coderef-description-format-dictionary
  '(("Japanese" . "%s行目")))

(defcustom org-link-completion-default-coderef-description-format
  (alist-get
   ;; Should I use `org-export-default-language'? (Is it loaded at this point?)
   current-language-environment
   org-link-completion-default-coderef-description-format-dictionary
   "Line %s"
   nil #'equal)
  "Default description format for coderef links."
  :group 'org-link-completion
  :type 'string)

(defun org-link-completion-default-coderef-description (label)
  (format org-link-completion-default-coderef-description-format label))


(defun org-link-completion-strip-internal-link (path)
  "Strip prefix and suffix (if any) at the beginning or end of internal links.

For example:
  \"#custom-id\" => \"custom-id\"
  \"*heading\" => \"heading\"
  \"(coderef)\" => \"coderef\""
  (let ((path-len (length path)))
    (or
     ;; #custom-id or *heading
     (and (>= path-len 1)
          (memq (elt path 0) '(?# ?*))
          (substring path 1))
     ;; (coderef)
     (and (>= path-len 2)
          (eq (elt path 0) ?\()
          (eq (elt path (1- path-len)) ?\))
          (substring path 1 -1))
     path)))

(defun org-link-completion-get-heading ()
  (when-let ((heading (org-get-heading t t t t)))
    (substring-no-properties heading)))


(defconst org-link-completion-escape-description-separator
  ;; export snippets hack or zero width space
  ;;(string ?\x200B)
  "@@-:@@")

(defun org-link-completion-escape-description-string (str)
  "Replace \"]]\" to \"]?]\"."
  ;; Ref: `org-link-make-string' (have a bug)
  (let ((last 0)
        curr
        (result ""))
    (while (setq curr (string-match "]\\(]\\|\\'\\)" str last))
      (setq result (concat result
                           (substring str last curr)
                           "]"
                           org-link-completion-escape-description-separator)
            last (1+ curr)))
    (setq result (concat result (substring str last)))
    result))

(defun org-link-completion-link-search (path)
  (ignore-errors
    (if (eq org-link-search-must-match-exact-headline 'query-to-create)
        ;; Suppress questions to users.
        (let ((org-link-search-must-match-exact-headline nil))
          (org-link-search path nil t))
      (org-link-search path nil t))
    t))


;;;; Utilities for completion

;;;;; Completion Table

(defun org-link-completion-table-with-metadata (table metadata-alist)
  (when table
    (lambda (string predicate action)
      (cond
       ((eq action 'metadata) (cons 'metadata metadata-alist))
       (t (complete-with-action action table string predicate))))))

(defun org-link-completion-table-keep-order (table)
  (when table
    (org-link-completion-table-with-metadata
     table
     '((display-sort-function . identity)))))

;;;;; Return value of completion-at-point-functions

(defun org-link-completion-capf-result (beg end table &rest plist)
  (when table
    (nconc (list beg end
                 (org-link-completion-table-keep-order table))
           (org-link-completion-capf-result-convert-properties plist))))

(defun org-link-completion-capf-result-convert-properties (plist)
  (cl-loop for (key value) on plist by #'cddr
           nconc
           (pcase key
             ;; :kind <symbol>|<function>
             (:kind
              (list :company-kind
                    (if (functionp value) value (lambda (_) value))))
             (_
              (list key value)))))

;;;;; Collectors

;; Collector returns only list of candidates

(defun org-link-completion-call-collectors (collectors)
  "Call functions in the list COLLECTORS and concatenate the
 returned lists with `nconc'."
  (mapcan (lambda (collector) (funcall collector)) collectors))

(defun org-link-completion-collect-stripped-internal-link-path ()
  "Collect a string with internal link symbols removed from the
 path of the link at point."
  (org-link-completion-parse-let nil (path)
    (list (org-link-completion-strip-internal-link path))))

(defun org-link-completion-collect-path ()
  "Collect a path of the link at point."
  (org-link-completion-parse-let nil (path)
    (list path)))

;;;;; Propertize

(defun org-link-completion-annotate (str annotation)
  (propertize str :org-link-completion-annotation annotation))

(defun org-link-completion-annotation (str)
  (get-text-property 0 :org-link-completion-annotation str))


;;;; Complete Path from Other Links

(defun org-link-completion-capf-path-from-other-links ()
  "Complete the path at point from other links."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-collect-path-from-other-links)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-path-from-other-links ()
  (org-link-completion-parse-let nil (type-beg type-end)
    (when (< type-beg type-end)
      (save-excursion
        (goto-char (point-min))
        (let ((re (concat
                   "\\[\\["
                   (regexp-quote
                    (buffer-substring-no-properties type-beg type-end))
                   ":"
                   "\\(\\(?:[^][\\]\\|\\\\[][\\]\\|\\\\[^][\\]\\)+\\)\\][][]"))
              table)
          (while (re-search-forward re nil t)
            (unless (= (+ (match-beginning 0) 2) type-beg)
              (let ((path (match-string-no-properties 1)))
                (unless (member path table)
                  (push path table)))))
          table)))))


;;;; Complete Description from Other Links

(defun org-link-completion-capf-desc-from-other-links ()
  "Complete the description at point from other links."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-collect-description-from-other-links)
     :kind 'text
     :annotation-function #'org-link-completion-annotation)))

(defun org-link-completion-collect-description-from-other-links (&optional
                                                                 link-beg
                                                                 link-end)
  (org-link-completion-parse-let nil (type-beg path-end)
    (unless link-beg (setq link-beg type-beg))
    (unless link-end (setq link-end path-end))
    (when (< link-beg link-end)
      (save-excursion
        (goto-char (point-min))
        (let ((re (concat "\\[\\["
                          (regexp-quote
                           (buffer-substring-no-properties link-beg link-end))
                          "\\]\\["
                          "\\(.*\\)\\]\\]")))
          (cl-loop while (re-search-forward re nil t)
                   unless (= (+ (match-beginning 0) 2) link-beg)
                   collect (match-string-no-properties 1)))))))


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
