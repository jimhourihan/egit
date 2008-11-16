;;; egit.el --- emacs git commit history interface ala gitk and qgit

;; Copyright (C) 2008 Jim Hourihan

;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This file contains an interface to git commands which are not
;; (currently) part of git.el. Primarily, this is a view of the git commit
;; history with the ability to mark ranges and/or single commits and
;; operate on them. Mutiple branches/tags/ref interfaces can be active at
;; the same time.
;;
;; To install: put this file on the load-path and place the following in
;; your .emacs file:
;;
;;      (require 'egit)
;;          -or-
;;      (autoload 'egit "egit" "Emacs git history" t)
;;      (autoload 'egit-file "egit" "Emacs git history file" t)
;;      (autoload 'egit-dir "egit" "Emacs git history directory" t)
;;
;; `M-x egit' shows commit history for a given branch, tag, or other
;; ref. You supply a directory (to identify the repo), the ref, and a
;; maximum number of commits to show.
;;
;; `M-x egit-file' will show commits related to a specific file
;;
;; `M-x egit-dir' will show commits related to files within a directory
;;
;; egit is currently useful for browsing the history. It has only a few
;; operations implemented (cherry-pick, revert, tag, delete tag) -- and
;; those are implemented only minimally. The framework for operating on a
;; single or multiple commits (that have been marked) already exists, so
;; adding new git commands should be relatively easy.
;;
;; There isn't a way to specify a range of commits (unless you use the
;; marked commits to do so). There should probably be a dedicated method of
;; creating a range to pass to git commands. It might be good to have a
;; facility like qgit's in which user defined commands can be added
;; (although the default set should be fairly complete)
;;
;; The "occur" feature needs something like himark to highlight the occur
;; regex in the comment buffer(s). There's a piece of code in there that
;; currently calls himark that's commented out. The idea is to "grep"
;; through comments. For example to highlight all cherry picked commits
;; search for "cherry", etc.
;;
;; Start up time can be significant when 10s of thousands of commits are
;; being displayed. Unfortunately, this is not an unusual situtation. 
;;
;; TODO
;;  - graph representation along the left of the commit buffer
;;  - graphical rep of a commit range (like the marks)
;;  - git reset functions
;;  - branch creation at a commit
;;  - patch creation from a set of commits
;;  - show list of authors/sign-offs related to commits
;;  - make occur more useful
;;  - speed up 
;;  - add "more" buttons to end of ewoc to load more commits
;;  - lazy resolve parent commits
;;  - make delete tag default to one of the current commit tags
;;
;; WISH
;;  - incrementally load commits 
;;

(require 'git)
(require 'ewoc)
(eval-when-compile (require 'cl))

(defvar egit-mode-map nil "Mode map for egit mode")
(defvar egit-mode-menu nil "Menu for egit mode")

(defface egit-base-face
  '((t (:inherit 'variable-pitch))) "egit: base"
  :group 'egit)

;(defface egit-base-face
  ;'((t ())) "egit: base"
  ;:group 'egit)
  
(defface egit-tag-face 
  '((t (:inherit 'egit-base-face :background "yellow" :box t))) "egit: tag"
  :group 'egit)

(defface egit-remote-face 
  '((t (:inherit 'egit-base-face :background "bisque" :box t))) "egit: remote"
  :group 'egit)

(defface egit-head-face 
  '((t (:inherit 'egit-base-face :background "green" :box t))) "egit: head"
  :group 'egit)

(defface egit-highlight-face 
  '((t (:inherit 'egit-base-face :background "darkseagreen1")))
  "egit: highlight"
  :group 'egit)

(defface egit-merged-commit-face
  '((t (:inherit 'egit-base-face :background "SkyBlue" :box t))) "egit: merged"
  :group 'egit)

(defface egit-marked-face
  '((t (:inherit 'egit-base-face :background "orange" :box t))) "egit: marked"
  :group 'egit)

(defface egit-date-face
  '((t (:inherit 'egit-base-face :foreground "grey25"))) "egit: date"
  :group 'egit)

(defface egit-subdued-date-face
  '((t (:inherit 'egit-base-face :foreground "grey60"))) "egit: date subdued"
  :group 'egit)

(defface egit-id-face
  '((t (:inherit 'fixed-pitch))) "egit: id"
  :group 'egit)

(defface egit-file-name-face
  '((t (:inherit 'default :foreground "blue"))) "egit: file-name"
  :group 'egit)

(defface egit-heading-face
  '((t (:inherit 'bold))) "egit: heading"
  :group 'egit)

(defface egit-more-face
  '((t (:inherit 'egit-base-face :foreground "blue"))) "egit: more"
  :group 'egit)

(defface egit-diff-diff-face 
  '((t (:foreground "blue4" :background "lavender"))) "egit: diff diff"
  :group 'egit)

(defface egit-diff-minus-face '((t (:foreground "red3"))) "egit: diff minus"
  :group 'egit)

(defface egit-diff-plus-face '((t (:foreground "green3"))) "egit: diff plus"
  :group 'egit)

(defface egit-diff-graph-plus-face
  '((t (:background "green3" :foreground "green3"))) "egit: graph diff plus"
  :group 'egit)

(defface egit-diff-graph-minus-face
  '((t (:background "red3" :foreground "red3"))) "egit: graph diff minus"
  :group 'egit)

(defface egit-diff-context-face '((t (:foreground "magenta4"))) "egit: diff context"
  :group 'egit)

(defvar egit-current-line-face-overlay '(:background "grey70"))

(defstruct egit--commit
  id
  tag
  refs
  parents
  children
  subject
  author
  date 
  merge
  comments
  mark)

(defvar egit-log-buffer " *egit-log*")
(defvar egit-branch-buffer " *egit-branch*")
(defvar egit-tag-buffer " *egit-tags*")
(defvar egit-commit-buffer "*egit-commit*")
(defvar egit-diff-buffer "*egit-commit-diff*")
(defvar egit-temp-buffer " *egit-temp*")

(defun egit-parse-commit-line (line commit)
  (let ((w (car (split-string line)))
        (c (> (length line) 1)))
    (cond 
     ((and c (string= w "Date:")) (setf (egit--commit-date commit)
                                        (date-to-time (substring line 8 -1))))
     ((and c (string= w "Author:")) (setf (egit--commit-author commit) (substring line 8 -1)))
     ((and c (string= w "Merge:")) (setf (egit--commit-merge commit) (substring line 7 -1)))
     ((and c (string-match "^commit " line)) nil)
     (t 
      (let ((comments (egit--commit-comments commit))
            (subject (egit--commit-subject commit)))
        (when (and w (not subject))
          (setf (egit--commit-subject commit) (substring line 4 -1)))
        (when (or comments w)
          (setf (egit--commit-comments commit) 
                (cons line comments)))
        t)))))


(defun egit-heading (s)
  (propertize s 'face 'egit-heading-face))

(defun egit-propertize-ref (s)
  (cond 
   ((string-match "refs/heads/\\(.*\\)" s)
      (propertize (match-string 1 s) 'face 'egit-head-face))
   ((string-match "refs/tags/\\(.*\\)" s)
      (propertize (match-string 1 s) 'face 'egit-tag-face))
   ((string-match "refs/remotes/\\(.*\\)" s)
      (propertize (match-string 1 s) 'face 'egit-remote-face))
   (t nil)))

(defun egit-combine-subject-refs (commit)
  (let ((subject-text (egit--commit-subject commit))
        (reflist (egit--commit-refs commit)))
    (if subject-text
        (if (and reflist (sequencep reflist))
            (progn
              (let ((s ""))
                (dolist (x reflist)
                  (let ((q (egit-propertize-ref x)))
                    (when q (setq s (concat s q " ")))))
                (setf (egit--commit-subject commit) 
                      (concat 
                       s 
                       (propertize subject-text 'face 'egit-base-face)))))
          (setf (egit--commit-subject commit) (propertize subject-text 'face 'egit-base-face)))
      (setf (egit--commit-subject commit) (propertize "No Commit Message" 'face 'italic)))))

(defun egit-parse-commit-id (text commit)
  (let* ((parts (split-string text "[()\n\r]"))
         (line (split-string (car parts)))
         (refline (car (cdr parts)))
         (sha1 (car (cdr line)))
         (rents (if (cdr line) (cdr (cdr line)) nil)))
    (setf (egit--commit-id commit) sha1)
    (setf (egit--commit-parents commit) rents)
    (setf (egit--commit-children commit) nil)
    (when (and refline (not (string= "" refline)))
      (setf (egit--commit-refs commit) (split-string refline "[ ,]")))))

(defun egit-parse-commit ()
  "Requires that point is at the beginning of a commit log entry"
  (interactive)
  (let* ((line (thing-at-point 'line))
         (commit-line (cdr (split-string line)))
         (commit (make-egit--commit)))
    (egit-parse-commit-id line commit)
    (forward-line)
    (beginning-of-line)
    (while (and (not (eobp))
                (egit-parse-commit-line (thing-at-point 'line) commit))
      (forward-line)
      (beginning-of-line))
    (egit-combine-subject-refs commit)
    (setf (egit--commit-comments commit) (reverse (egit--commit-comments commit)))
    commit))

(defun egit-parse-log (ref-start &optional n)
  "Read commit log"
  (if (and n (> n 0))
      (git-run-command-buffer egit-log-buffer 
                              "log" "--parents" "--decorate" 
                              (format "-%d" n) ref-start)
    (git-run-command-buffer egit-log-buffer 
                            "log" "--parents" "--decorate" ref-start))
  (save-excursion
    (let ((buffer (get-buffer-create egit-log-buffer))
          (commits nil))
      (set-buffer buffer)
      (let ((last-pcent-done 0)
            (pcent-down 0)
            (start-time (float-time))
            (total (- (point-max) (point-min))))
        (goto-char (point-min))
        (while (not (eobp))
          (when (> (- (float-time) start-time) 3.0)
            (setq pcent-done (/ (point) (/ total 100)))
            (unless (equal pcent-done last-pcent-done)
              (setq last-pcent-done pcent-done)
              (message "Parsed %d%% ..." pcent-done)))
          (setq commits (cons (egit-parse-commit) commits)))
        (kill-buffer buffer)
        (reverse commits)))))

(defun egit-parse-file-log (file)
  "Read commit log for a specific file"
  (git-run-command-buffer egit-log-buffer 
                          "log" "--decorate" "--follow" "--parents" file)
  (save-excursion
    (let ((buffer (get-buffer-create egit-log-buffer))
          (commits nil))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (not (eobp))
        (setq commits (cons (egit-parse-commit) commits)))
      ;(kill-buffer buffer)
      (reverse commits))))

(defun max-number (list biggest)
  (if list
      (let ((x (car list))
            (rest (cdr list)))
        (if (> x biggest) 
            (max-number rest x)
          (max-number rest biggest)))
    biggest))

(defun egit-get-selection ()
  "Return a list of marked commits (or the current if none are marked)"
  (interactive)
  (let ((commits nil)) 
    (dolist (c egit-commits)
      (when (egit--commit-mark c)
        (setq commits (cons c commits))))
    (if commits
        commits
      (list (ewoc-data (ewoc-locate egit-ewoc))))))

(defun egit-get-tags ()
  "Returns a list of tags"
  (interactive)
  (git-run-command-buffer egit-tag-buffer "tag" "-l")
  (with-current-buffer egit-tag-buffer
    (goto-char (point-min))
    (let ((tags nil))
      (while (< (point) (point-max))
        (setq tags (cons (substring (thing-at-point 'line) 0 -1) tags))
        (forward-line))
      (kill-buffer egit-tag-buffer)
      tags)))

(defun egit-get-branches ()
  "Returns a list: (current-branch (b1 b2 b3 ...)) including remote tracking branches"
  (interactive)
  (git-run-command-buffer egit-branch-buffer "branch" "-a")
  (with-current-buffer egit-branch-buffer
    (goto-char (point-min))
    (let ((current-branch nil)
          (branches nil))
      (while (< (point) (point-max))
        (setq branches 
              (cons
               (let ((line (thing-at-point 'line)))
                 (string-match "\\([* ]\\) \\(.*\\)" line)
                 (let ((star (match-string 1 line))
                       (branch (match-string 2 line)))
                   (when (string= star "*")
                     (setq current-branch branch))
                   branch))
               branches))
        (forward-line))
      (kill-buffer egit-branch-buffer)
      (list current-branch branches))))

(defun egit-populate-ewoc (ewoc commits)
  "Popuate the ewoc node structure with commits"
  (let ((n (length commits))
        (i 0))
    (dolist (c commits)
      (setq i (+ i 1))
      (ewoc-enter-last ewoc c))))

(defun egit-day-string (commit)
  (format-time-string "%Y-%m-%d" (egit--commit-date commit)))

(defun egit-same-day-commit (commit other)
  "Returns t if the commit and the other commit are from the same day"
  (if (or (stringp other) (stringp commit))
      nil
    (string= (egit-day-string commit)
             (egit-day-string other))))

(defun egit-pretty-printer (commit)
  "Display a commit for ewoc"
  (let* ((s (egit--commit-subject commit))
         (refs (egit--commit-refs commit))
         (mark (egit--commit-mark commit))
         (date (egit--commit-date commit))
         (v s))
    (when egit-highlight-regex
      (dolist (l (egit--commit-comments commit))
        (when (string-match egit-highlight-regex l)
          (setq v (propertize v 'face 'egit-highlight-face)))))
    (when egit-show-id
      (setq v
            (concat (propertize (egit--commit-id commit) 'face 'egit-id-face)
                    " "
                    v)))
    (when egit-show-date
      (let* ((parents (egit--commit-parents commit))
             (same-day (and (= 1 (length parents))
                            (egit-same-day-commit (car parents) commit))))
        (setq v 
              (concat 
               (propertize (format-time-string "%Y-%m-%d" date) 'face
                           (if same-day 'egit-subdued-date-face
                             'egit-date-face))
               (propertize (format-time-string " %T" date) 'face 'egit-date-face)
               " "
               v))))
    (insert (concat (if mark "* " "  ") v))))

(defun egit-current-line-decoration ()
  (move-overlay egit-current-overlay 
                (line-beginning-position) 
                (+ 1 (line-end-position))))
  

(defun egit-next-line (&optional n)
  "Move forward one line"
  (interactive "P")
  (goto-char (line-beginning-position))
  (forward-line)
  (egit-show-current-line-info))

(defun egit-previous-line (&optional n)
  "Move backward one line"
  (interactive "P")
  (goto-char (line-beginning-position))
  (forward-line -1)
  (egit-show-current-line-info))

(defun egit-show-commit-date ()
  "Show commit dates in the commit history"
  (interactive)
  (setq egit-show-date (if egit-show-date nil t))
  (save-excursion
    (ewoc-refresh egit-ewoc)))

(defun egit-show-commit-id ()
  "Show commit id in the commit history"
  (interactive)
  (setq egit-show-id (if egit-show-id nil t))
  (save-excursion
    (ewoc-refresh egit-ewoc)))

(defun egit-show-all-commit ()
  "Show commit with all of the comments"
  (interactive)
  (egit-clear-other-windows)
  (egit-show-commit t))

(defun egit-quit ()
  "Quit egit kill buffer"
  (interactive)
  (egit-clear-other-windows)
  (kill-buffer (current-buffer)))

(defun egit-delete-window-buffer (buffer-name)
  (let* ((old-buffer (get-buffer buffer-name))
	 (old-window (get-buffer-window buffer-name)))
    (if old-window (delete-window old-window))
    (if old-buffer (kill-buffer old-buffer))))

(defun egit-clear-other-windows ()
  "Nuke temporary windows for better viewing"
  (interactive)
  (save-excursion
    (egit-delete-window-buffer egit-diff-buffer)
    (egit-delete-window-buffer egit-temp-buffer)
    (egit-delete-window-buffer egit-commit-buffer)))

(defun egit-show-commit (long)
  "Show commit by ref/hash"
  (let* ((node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node))
         (highlight-regex egit-highlight-regex)
         (author (egit--commit-author commit))
         (sha1 (egit--commit-id commit))
         (parents (egit--commit-parents commit))
         (comments (egit--commit-comments commit))
         (ncomments (length comments))
         (summary (egit--commit-subject commit))
         (merge (egit--commit-merge commit))
         (date (format-time-string "%b %d %T %Y %z" (egit--commit-date commit))))
    (egit-clear-other-windows)
    (let ((buffer (get-buffer-create egit-commit-buffer)))
      (set-buffer buffer)
      (buffer-disable-undo)
      (goto-char (point-min))
      (kill-region (point-min) (point-max))
      (insert (concat (egit-heading "   SHA1: ") sha1 "\n"))
      (when parents 
        (dolist (p parents)
          (insert (concat (egit-heading " Parent: ")
                          ;; its either a commit or a (unresolved commit) string
                          (if (typep p 'egit--commit)
                              (egit--commit-subject p)
                            p)
                          "\n"))))
      (when merge
        (insert (concat (egit-heading "  Merge: ") merge "\n")))
      (when (> ncomments 2)
        (setq summary 
              (concat summary 
                      (propertize 
                       (format " + More (%d lines)..." (- (length comments) 1))
                       'face 'egit-more-face))))
      (insert (concat (egit-heading " Author: ") author "\n"
                      (egit-heading "   Date: ") date "\n"
                      (egit-heading "Summary: ") summary))
      (when long
        (insert "\n-\n")
        (dolist (c comments)
          (insert c)))
      (goto-char (point-min))
      ;(when highlight-regex
      ;  (himark-regexp highlight-regex))
      (setq buffer-read-only t)
      (let* ((window (get-largest-window))
             (new-window (split-window window)))
        (set-window-buffer new-window buffer)
        (select-window new-window)
        (resize-temp-buffer-window)
        (select-window window)))))

(defun egit-show-files-commit ()
  "Show files changed by commit"
  (interactive)
  (let* ((node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node)))
    (egit-clear-other-windows)
    (let ((buffer 
           (git-run-command-buffer 
            egit-diff-buffer "show" "--pretty=short" "--stat" (egit--commit-id commit))))
      (with-current-buffer egit-diff-buffer
        (egit-diff-mode)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (let* ((window (get-largest-window))
               (new-window (split-window window)))
          (set-window-buffer new-window buffer)
          (select-window new-window)
          (resize-temp-buffer-window)
          (select-window window))))))

(defun egit-show-diff-commit ()
  "Show diff of a commit"
  (interactive)
  (let* ((node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node)))
    (egit-clear-other-windows)
    (let ((buffer 
           (git-run-command-buffer egit-diff-buffer 
                                   "show" "-p" "--stat" (egit--commit-id commit))))
      (with-current-buffer egit-diff-buffer
        (egit-diff-mode)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (let* ((window (get-largest-window))
               (new-window (split-window window)))
          (set-window-buffer new-window buffer)
          (select-window new-window)
          (resize-temp-buffer-window)
          (select-window window))))))

(defun egit-mark ()
  "Mark active commit"
  (interactive)
  (let* ((node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node)))
    (setf (egit--commit-mark commit) t)
    (ewoc-invalidate egit-ewoc node)
    (egit-next-line)))

(defun egit-unmark ()
  "Unmark active commit"
  (interactive)
  (let* ((node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node)))
    (setf (egit--commit-mark commit) nil)
    (ewoc-invalidate egit-ewoc node)
    (egit-next-line)))

(defun egit-mouse-click (event)
  "Click in the egit buffer"
  (interactive "e")
  (let* ((node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node))
         (event-type (car event))
         (position (car (cdr event)))
         (clicks (car (cdr (cdr event)))))
    (goto-char (line-beginning-position))
    (egit-show-current-line-info)))

(defun egit-cherry-pick (doit)
  "Cherry pick commits"
  (interactive
   (let* ((branch-state (egit-get-branches))
          (current-branch (car branch-state)))
     (if (not (string= current-branch egit-top))
         (list
          (y-or-n-p
           (concat "Cherry-Pick to " 
                   (propertize current-branch 'face 'egit-head-face) " ")))
       (list 'bad))))
  (let ((commits (egit-get-selection)))
    (if (and doit (not (eq doit 'bad)))
        (dolist (c commits)
          (egit-clear-other-windows)
          (let ((buffer (git-run-command-buffer egit-temp-buffer
                                                "cherry-pick" "-x"
                                                (egit--commit-id c))))
            (with-current-buffer buffer
              (let* ((window (get-largest-window))
                     (new-window (split-window window)))
                (set-window-buffer new-window buffer)
                (select-window new-window)
                (resize-temp-buffer-window)
                (select-window window))
              ))))
      (if (eq doit 'bad)
          (message "Don't cherry pick off the current branch")
        (message "Cancelled"))))

(defun egit-revert (doit)
  "Revert commits without commiting them"
  (interactive
   (let* ((branch-state (egit-get-branches))
          (current-branch (car branch-state))
          (commits (egit-get-selection))
          (ncommits (length commits)))
     (if (string= current-branch egit-top)
         (list
          (y-or-n-p
           (format "Revert %d commit%s (without committing)" 
                   ncommits
                   (if (> ncommits 1) "s" ""))))
       (list 'bad))))
  (let ((commits (egit-get-selection)))
    (if (and doit (not (eq doit 'bad)))
        (dolist (c commits)
          (egit-clear-other-windows)
          (let ((buffer (git-run-command-buffer egit-temp-buffer
                                                "revert" "-n"
                                                (egit--commit-id c))))
            (with-current-buffer buffer
              (let* ((window (get-largest-window))
                     (new-window (split-window window)))
                (set-window-buffer new-window buffer)
                (select-window new-window)
                (resize-temp-buffer-window)
                (select-window window))
              ))))
      (message "Cancelled")))

(defun egit-tag (name)
  "Tag the current commit"
  (interactive
   (let* ((tags (egit-get-tags))
          (name (completing-read "Tag Name: " nil))
          (exists (member name tags)))
     (if (or (and exists
                  (y-or-n-p (concat name " is already a tag. Force")))
             (not exists))
         (list name)
       nil)))
  (let* ((ann (read-string "Annotation: "))
         (node (ewoc-locate egit-ewoc))
         (commit (ewoc-data node))
         (buffer (git-run-command-buffer egit-temp-buffer 
                                         "tag" 
                                         "-a" name 
                                         (egit--commit-id commit) 
                                         "-m" ann)))
    (with-current-buffer buffer
      (let* ((window (get-largest-window))
             (new-window (split-window window)))
        (set-window-buffer new-window buffer)
        (select-window new-window)
        (resize-temp-buffer-window)
        (select-window window))))
  (egit-refresh))

(defun egit-delete-tag (name)
  "Delete a tag"
  (interactive
   (let* ((tags (egit-get-tags))
          (name (completing-read "Delete Tag: " tags nil t)))
     (list name)))
  (let ((buffer (git-run-command-buffer egit-temp-buffer 
                                        "tag" 
                                        "-d" name)))
    (with-current-buffer buffer
      (let* ((window (get-largest-window))
             (new-window (split-window window)))
        (set-window-buffer new-window buffer)
        (select-window new-window)
        (resize-temp-buffer-window)
        (select-window window))))
  (egit-refresh))

(defun egit-show-current-line-info ()
  "Show current commit info"
  (interactive)
  (egit-current-line-decoration)
  (egit-show-commit nil))

(defvar egit-mode-map nil
  "Keymap for egit major mode.")

(defun egit-refresh ()
  "Refresh current egit buffer"
  (interactive)
  (let* ((dir egit-repo-dir)
         (ref egit-top)
         (commits 
          (if egit-log-file
              (egit-parse-file-log (expand-file-name egit-log-file))
               (egit-parse-log ref egit-max-commits))))
    (setq buffer-read-only nil)
    (egit-clear-other-windows)
    (egit-mode commits ref dir egit-max-commits egit-log-file)))

(defun egit-occur (regex)
  "Highlight commits whose comments match a regular expression"
  (interactive "sSearch Regex: ")
  (save-excursion
    (setq egit-highlight-regex 
          (if (string= regex "") nil regex))
    (ewoc-refresh egit-ewoc)))

(unless nil ; egit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
      (define-key map "\C-n" 'egit-next-line)
      (define-key map "\C-p" 'egit-previous-line)
      (define-key map "n" 'egit-next-line)
      (define-key map "p" 'egit-previous-line)
      (define-key map "f" 'egit-show-files-commit)
      (define-key map "c" 'egit-cherry-pick)
      (define-key map "r" 'egit-refresh)
      (define-key map "g" 'egit-refresh)
      (define-key map "d" 'egit-show-diff-commit)
      (define-key map "q" 'egit-quit)
      (define-key map "m" 'egit-mark)
      (define-key map "u" 'egit-unmark)
      (define-key map "o" 'egit-occur)
      (define-key map "D" 'egit-show-commit-date)
      (define-key map "i" 'egit-show-commit-id)
      (define-key map "t" 'egit-tag)
      (define-key map "T" 'egit-delete-tag)
      (define-key map "" 'egit-show-all-commit)
      (define-key map [mouse-1] 'egit-mouse-click)
      (define-key map [up] 'egit-previous-line)
      (define-key map [down] 'egit-next-line)
      (define-key map [right] 'egit-show-all-commit)
      (define-key map [left] 'egit-show-current-line-info)
      (setq egit-mode-map map))
  (easy-menu-define egit-menu egit-mode-map
    "EGit Menu"
    `("EGit"
      ["Display Date"    egit-show-commit-date [:selected '(lambda () egit-show-date)]]
      ["Display SHA1"    egit-show-commit-id [:selected '(lambda () egit-show-id)]]
      "--------"
      ["Show Comments"   egit-show-all-commit t]
      ["Show Diffs"      egit-show-diff-commit t]
      ["Show Files"      egit-show-files-commit t]
      "--------"
      ["Tag"             egit-tag t]
      ["Delete Tag"      egit-delete-tag t]
      "--------"
      ["Revert"          egit-revert t]
      ["Cherry-Pick"     egit-cherry-pick t]
      "--------"
      ["Mark"            egit-mark t]
      ["Unmark"          egit-unmark t]
      ["Search/Highlight"   egit-occur t]
      "--------"
      ["Refersh"         egit-refresh t]
      "--------"
      ["Quit"            egit-quit t]
      )))

(defun egit-resolve-parent (commit)
  (or (gethash commit egit-hash-map)
      commit))

(defun egit-largest-commit-subject (commits)
  "Find the largest line in the commit subjects"
  (let ((max 0))
    (dolist (c commits)
      (let* ((subject (egit--commit-subject c))
             (l (length subject)))
        (when (> l max)
          (setq max l))))
    max))

(defun egit-mode (commits ref dir n &optional file)
  "Mode for git commit logs"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (make-local-variable 'egit-commits)
  (make-local-variable 'egit-top)
  (make-local-variable 'egit-log-file)
  (make-local-variable 'egit-ewoc)
  (make-local-variable 'egit-current-overlay)
  (make-local-variable 'egit-repo-dir)
  (make-local-variable 'egit-highlight-regex)
  (make-local-variable 'egit-hash-map)
  (make-local-variable 'egit-max-commits)
  (make-local-variable 'egit-max-subject-length)
  (make-local-variable 'egit-show-date)
  (make-local-variable 'egit-show-id)
  (erase-buffer)
  (setq egit-hash-map (make-hash-table :test 'equal
                                       :size (length commits)))
  ; put all of the commits in a hash table keyed on sha1
  (message "Creating commit hash table...")
  (dolist (c commits)
    (puthash (egit--commit-id c) c egit-hash-map))
  ; resolve the parent sha1 strings to actual commit objects
  ; pehaps this should be done lazily since its only used to show parent
  ; subjects right now. Eventually this info can be used (with children) to
  ; draw a graph in the buffer
  (message "Resolving commit parents...")
  (dolist (c commits)
    (let ((resolved-parents (mapcar 'egit-resolve-parent
                                    (egit--commit-parents c))))
      (setf (egit--commit-parents c) resolved-parents))
  ; make a child list while we're at it
  ;(dolist (p (egit--commit-parents c))
  ; (setf (egit--commit-children p)
  ;       (cons c (egit--commit-children p))))
    )
  (setq egit-commits commits
        egit-top ref
        egit-repo-dir dir
        egit-max-commits n
        egit-log-file file
        egit-show-cherry-picked nil
        egit-highlight-regex nil
        egit-show-date nil
        egit-show-id nil
        egit-max-subject-length (egit-largest-commit-subject commits)
        egit-ewoc (ewoc-create 'egit-pretty-printer)
        egit-current-overlay (make-overlay 0 0))
  (overlay-put egit-current-overlay 'face egit-current-line-face-overlay)
  (egit-populate-ewoc egit-ewoc commits)
  (setq header-line-format
        (concat
         (propertize "   egit commit summary -- user: " 'face 'egit-base-face)
         (propertize (git-get-committer-name) 'face 'bold)
         (propertize ", top: " 'face 'egit-base-face)
         (propertize egit-top 'face 'bold)
         (if egit-log-file
             (concat 
              (propertize ", file: " 'face 'egit-base-face)
              (propertize egit-log-file 'face 'bold))
           "")
         ))
  (let ((ncommits (length commits)))
    (ewoc-set-hf egit-ewoc 
                 (concat (egit-heading "Repository:") " " dir "\n"
                         (if egit-log-file
                             (concat (egit-heading "  File/Dir:") " "
                                     egit-log-file "\n")
                           "")
                         (egit-heading "       Top:") " " egit-top ", " 
                         (format "%d commit%s\n" ncommits
                                 (if (> ncommits 1) "s" "")) 
                         )
                 (if (and egit-max-commits (> egit-max-commits 0))
                     (format (propertize "\n-- Limited to last %d commits --"
                                         'face 'egit-more-face)
                             egit-max-commits)
                   "")))
  (message "Refreshing egit-ewoc...")
  (ewoc-refresh egit-ewoc)
  (if (not truncate-lines) (toggle-truncate-lines))
  (goto-char (point-min))
  (forward-line 2)
  (setq major-mode 'egit-mode
        mode-name "egit"
        goal-column 1)
  (put 'egit-mode 'mode-class 'special)
  (use-local-map egit-mode-map)
  (setq buffer-read-only t))

(defun egit-dir (dir)
  "Start up egit limited to directory"
  (interactive
   (list
    (let ((dir (file-name-directory buffer-file-name)))
      (if buffer-file-name
          (read-directory-name
           "Egit log of directory: " 
           dir dir t nil)
      (read-directory-name
       "Egit log of directory: " nil nil t nil)))))
  (egit-file dir))

(defun egit-file (file)
  "Start up egit limited to file (or directory)"
  (interactive
   (list
    (if buffer-file-name
        (read-file-name 
         "Egit log of file/directory: " 
         (file-name-directory buffer-file-name)
         buffer-file-name 
         t
         (file-name-nondirectory buffer-file-name))
      (read-file-name
       "Egit log of file/directory: " nil nil t))))
  (let ((buffer (get-buffer-create 
                 (format "*egit:%s*" (file-name-nondirectory file)))))
    (switch-to-buffer buffer)
    (let ((dir (git-get-top-dir (file-name-directory file))))
      (cd dir)
      (let* ((branch-state (egit-get-branches))
             (current-branch (car branch-state)))
        (egit-mode (egit-parse-file-log 
                    (expand-file-name file)) current-branch dir 0 file)))))

(defun egit (dir ref n)
  "Start up egit for DIR on REF (a branch, tag, or other ref) with at most
N commits shown"
  (interactive
   (with-temp-buffer
     (list 
      (let ((dir (read-directory-name "Directory in repo: " nil nil t nil)))
        (cd dir)
        dir)
      (let* ((branch-state (egit-get-branches))
             (current-branch (car branch-state))
             (all-tags (egit-get-tags))
             (all-branches (car (cdr branch-state)))
             (all-refs (append all-branches all-tags)))
        (completing-read 
         (concat "Ref (" current-branch "): ") 
         all-refs
         nil nil nil nil current-branch))
      (read-number "Max number of commits to show or 0 for all " 0))))
  (let ((buffer (get-buffer-create (format "*egit:%s*" ref))))
    (switch-to-buffer buffer)
    (cd (git-get-top-dir dir))
    (egit-mode (egit-parse-log ref n) ref dir n)))


(setq egit-diff-font-lock-keywords
'( 
  ("^\\(commit\\)" (1 'egit-heading-face))
  ("^\\(Author:\\)" (1 'egit-heading-face))
  ("^\\(Date:\\)" (1 'egit-heading-face))
  ("^\\(diff --git.*\\)$" (1 'egit-diff-diff-face))
  ("^\\(-.*\\)$" (1 'egit-diff-minus-face))
  ("^\\(\\+.*\\)$" (1 'egit-diff-plus-face))
  ("^ \\([^ ][^|]+\\)| +" (1 'egit-file-name-face))
  (".+ | +[^+]*\\(\\++\\)" (1 'egit-diff-graph-plus-face))
  (".+ | +[^-]*\\(-+\\)" (1 'egit-diff-graph-minus-face))
  ("[0-9]+ insertions(\\(\\+\\))" (1 'egit-diff-graph-plus-face))
  ("[0-9]+ deletions(\\(-\\))" (1 'egit-diff-graph-minus-face))
  ("^\\(@@.*\\)$" (1 'egit-diff-context-face))))

;"Default expressions to highlight in egit-diff mode."))

(define-derived-mode egit-diff-mode text-mode "egit-diff"
  "Major mode for font-locking egit diff output"
  (modify-syntax-entry ?\'  ".")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|>" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'(egit-diff-font-lock-keywords 
	  nil				; KEYWORDS-ONLY: no
	  nil				; CASE-FOLD: no
	  ((?_ . "w"))			; SYNTAX-ALIST
	  )))

(provide 'egit)
