;;; egit.el --- emacs git commit history interface ala gitk and qgit

;; Copyright (C) 2008 Jim Hourihan

;; Version: 1.1

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
;; `M-x egit' shows commit history for a given branch, tag, or other ref. With
;; a prefix argument, you supply a directory (to identify the repo), the ref,
;; and a maximum number of commits to show -- e.g., M-x C-u egit 
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
;; being displayed so the default is 500. You can increase that after the
;; fact by clicking on the number of commits message at the end of the
;; buffer. 
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
(defvar egit-default-max-commits 500 "Default number of commits to show")

(defface egit-base-face
  '((t (:inherit 'variable-pitch))) "egit: base"
  :group 'egit)

;(defface egit-base-face
  ;'((t ())) "egit: base"
  ;:group 'egit)

(defface egit-bisect-unknown-face 
  '((t (:inherit 'fixed-pitch :background "light blue" :box nil))) 
  "egit: bisect unknown"
  :group 'egit)

(defface egit-bisect-good-face 
  '((t (:inherit 'fixed-pitch :background "pale green" :box nil))) 
  "egit: bisect good"
  :group 'egit)

(defface egit-bisect-bad-face 
  '((t (:inherit 'fixed-pitch :background "tomato" :box nil))) 
  "egit: bisect bad"
  :group 'egit)
  
(defface egit-tag-face 
  '((t (:inherit 'egit-base-face :background "yellow" :box t))) "egit: tag"
  :group 'egit)

(defface egit-remote-face 
  '((t (:inherit 'egit-base-face :background "bisque" :box t))) "egit: remote"
  :group 'egit)

(defface egit-head-face 
  '((t (:inherit 'egit-base-face :background "green" :box t))) "egit: head"
  :group 'egit)

(defface egit-merge-base-face 
  '((t (:inherit 'egit-base-face :background "green3" :box t))) "egit: merge-base"
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

(defface egit-author-face
  '((t (:inherit 'fixed-pitch :foreground "grey25"))) "egit: date"
  :group 'egit)

(defface egit-subdued-author-face
  '((t (:inherit 'fixed-pitch :foreground "grey60"))) "egit: date subdued"
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

(defface egit-more-mouse-face
  '((t (:inherit 'egit-more-face :underline t))) "egit: more mouse"
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
  merge-bases
  refs
  parents
  children
  subject
  author
  date 
  merge
  comments
  bisect-state
  mark)

(defvar egit-log-buffer " *egit-log*")
(defvar egit-branch-buffer " *egit-branch*")
(defvar egit-tag-buffer " *egit-tags*")
(defvar egit-commit-buffer "*egit-commit*")
(defvar egit-diff-buffer "*egit-commit-diff*")
(defvar egit-temp-buffer " *egit-temp*")

(defun egit-parse-commit-line (line commit)
  (let ((w (car (split-string line)))
        (c (> (length line) 1))
        (nwsp (not (equal (aref line 0) ? ))))
    (cond 
     ((and nwsp c (string= w "Date:")) 
      (setf (egit--commit-date commit) (date-to-time (substring line 8 -1))))
     ((and nwsp c (string= w "Author:")) 
      (setf (egit--commit-author commit) (substring line 8 -1)))
     ((and nwsp c (string= w "Merge:")) 
      (setf (egit--commit-merge commit) (substring line 7 -1)))
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

(defun egit-nice-human-name (n)
  (string-match "\\(.*\\)<.*>" n)
  (let* ((N 12)
         (person (match-string 1 n))
         (parts (split-string person))
         (abbrev (mapconcat 'concat parts " "))
         (l (length abbrev)))
    (if (> l N)
        (substring abbrev 0 N)
      (concat (make-string (- N l) ? ) abbrev))))

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
                              "log" "--parents" "--decorate=full" 
                              (format "-%d" n) ref-start)
    (git-run-command-buffer egit-log-buffer 
                            "log" "--parents" "--decorate=full" ref-start))
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
                          "log" "--decorate=full" "--follow" "--parents" file)
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

(defun egit-update-ewoc ()
  (let ((node (ewoc-locate egit-ewoc)))
    (ewoc-refresh egit-ewoc)
    (ewoc-goto-node egit-ewoc node))
  (egit-current-line-decoration))

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

(defun egit-get-full-name (infile)
  "Returns the git full name for FILE"
  (let ((file (expand-file-name infile)))
    (git-run-command-buffer egit-temp-buffer "ls-files" "-z" "--full-name" "--" file)
    (with-current-buffer egit-temp-buffer
      (goto-char (point-min))
      (replace-string " " "\n")
      (goto-char (point-min))
      (car (split-string (buffer-string))))))

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

(defun egit-get-merge-base (branch current-branch)
  "Returns a pair (branch . sha1)"
  (git-run-command-buffer egit-temp-buffer "merge-base" branch current-branch)
  (with-current-buffer egit-temp-buffer
    (cons (substring (buffer-string) 0 -1) branch)))

(defun egit-get-merge-bases ()
  "Returns a list of pairs ((branch . id) ...)"
  (interactive)
  (let* ((branches (egit-get-branches ""))
         (current-branch (car branches))
         (other-branches (car (cdr branches)))
         (ids nil))
    (dolist (b other-branches)
      (when (not (string= b egit-top))
        (setq ids (cons (egit-get-merge-base b egit-top) ids))))
    ids))

(defun egit-update-merge-bases ()
  "Sets merge base info on existing ewoc structures"
  (interactive)
  (let ((merge-bases (egit-get-merge-bases)))
    (dolist (c egit-commits)
      (dolist (a merge-bases)
        (when (string= (egit--commit-id c) (car a))
          (setf (egit--commit-merge-bases c) 
                (cons (cdr a) (egit--commit-merge-bases c))))))))

(defun egit-get-branches (&optional local-only)
  "Returns a list: (current-branch (b1 b2 b3 ...)) including remote tracking branches"
  (interactive "M")
  (if local-only
      (git-run-command-buffer egit-branch-buffer "branch")
    (git-run-command-buffer egit-branch-buffer "branch" "-a"))
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

(defun egit-create-temp-buffer-and-window (buffer)
  (let* ((window (get-largest-window))
         (new-window (split-window window)))
    (set-window-buffer new-window buffer)
    (select-window new-window)
    (fit-window-to-buffer new-window 20)
    (select-window window)))

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
         (merge-bases (egit--commit-merge-bases commit))
         (date (egit--commit-date commit))
         (bisect-state (egit--commit-bisect-state commit))
         (author (egit--commit-author commit))
         (v s))
    (when (and egit-show-branch-points merge-bases)
      (dolist (b merge-bases)
        (setq v (concat (propertize (concat ">" b) 'face 'egit-merge-base-face) " " v))))
    (when egit-highlight-regex
      (dolist (l (egit--commit-comments commit))
        (when (string-match egit-highlight-regex l)
          (setq v (propertize v 'face 'egit-highlight-face)))))
    (when egit-bisect-mode
      (setq v 
            (concat 
             (cond 
              ((eql bisect-state 'good)
               (propertize "+" 'face 'egit-bisect-good-face))
              ((eql bisect-state 'bad) 
               (propertize "-" 'face 'egit-bisect-bad-face))
              (bisect-state 
               (propertize "?" 'face 'egit-bisect-unknown-face))
              )
             " " v)))
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
               (if egit-show-time
                   (propertize (format-time-string " %T" date) 'face 'egit-date-face)
                 "")
               " "
               v))))
    (when egit-show-author
      (setq v (concat (propertize (egit-nice-human-name author) 'face
                                  'egit-author-face)
                      " " 
                      v)))
    (insert (concat (if mark "* " "  ") v "\n"))))

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
  (egit-update-ewoc))

(defun egit-show-commit-time ()
  "Show commit times (and dates) in the commit history"
  (interactive)
  (setq egit-show-time (if egit-show-time nil t))
  (when egit-show-time (setq egit-show-date t))
  (egit-update-ewoc))

(defun egit-show-commit-author ()
  "Show commit author in the commit history"
  (interactive)
  (setq egit-show-author (if egit-show-author nil t))
  (egit-update-ewoc))

(defun egit-show-commit-branch-points ()
  "Show branch points from commits"
  (interactive)
  (setq egit-show-branch-points (if egit-show-branch-points nil t))
  (unless egit-branch-points
    (setq egit-branch-points t)
    (egit-update-merge-bases))
  (egit-update-ewoc))

(defun egit-show-commit-id ()
  "Show commit id in the commit history"
  (interactive)
  (setq egit-show-id (if egit-show-id nil t))
  (egit-update-ewoc))

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
      (egit-create-temp-buffer-and-window buffer))))

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
        (egit-create-temp-buffer-and-window buffer)))))

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
        (egit-create-temp-buffer-and-window buffer)))))

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
              (egit-create-temp-buffer-and-window buffer)))))
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
              (egit-create-temp-buffer-and-window buffer)
              ))))
      (message "Cancelled")))

(defun egit-checkout-file-from-history (file doit)
  "Checkout a file as it appears after a specific commit -- NOTE: this
can fail if the file had a different name in the past"
  (interactive
   (let* ((commits (egit-get-selection))
          (ncommits (length commits)))
     (if (= ncommits 1)
         (list 
          (if egit-log-file
              egit-log-file 
            (read-file-name 
             "File: " 
             egit-repo-dir
             egit-repo-dir
             t))
          t)
       (list 'bad))))
   (let ((commits (egit-get-selection))
         (top egit-top)
         (path (egit-get-full-name file)))
     (if (and doit (not (eq doit 'bad)))
         (dolist (c commits)
           (egit-clear-other-windows)
           (let ((buffer (git-run-command-buffer egit-temp-buffer
                                                 "cat-file" "blob" 
                                                 (concat
                                                  (egit--commit-id c) ":"
                                                  path))))
             (with-current-buffer buffer
               (let ((fname
                      (concat path ".~"
                              top "-"
                              (format-time-string "%b-%d-%T-%Y~"
                                                  (egit--commit-date
                                                   c)))))
                 (set-visited-file-name fname)
                 (goto-char (point-min))
                 (set-buffer-modified-p nil)
                 (egit-create-temp-buffer-and-window buffer)
                 ))))
       (message "Cancelled"))))
   

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
      (egit-create-temp-buffer-and-window buffer)))
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
      (egit-create-temp-buffer-and-window buffer)))
  (egit-refresh))

(defun egit-bisect-bad ()
  "Call git bisect good on the current commit"
  (interactive)
  (let* ((commits (egit-get-selection))
         (ncommits (length commits))
         (state 'bad))
    (unless egit-bisect-mode (egit-bisect-start))
    (dolist (c egit-commits)
      (when (eql state 'bad)
        (setf (egit--commit-bisect-state c) state))
      (if (eql c (car commits)) (setq state t)))
    (egit-update-ewoc)))

(defun egit-bisect-good ()
  "Call git bisect good on the current commit"
  (interactive)
  (let* ((commits (egit-get-selection))
         (ncommits (length commits))
         (state nil))
    (unless egit-bisect-mode (egit-bisect-start))
    (dolist (c egit-commits)
      (if (eql c (car commits)) (setq state 'good))
      (when (eql state 'good)
        (setf (egit--commit-bisect-state c) state))
      )
    (egit-update-ewoc)))

(defun egit-bisect-reset ()
  "Call git bisect reset"
  (interactive)
  (if (not egit-bisect-mode)
      (message "Not in bisect mode")
    (dolist (c egit-commits)
      (setf (egit--commit-bisect-state c) nil))
    (setq egit-bisect-mode nil)
    (egit-update-ewoc)))

(defun egit-bisect-start ()
  "Call git bisect start"
  (interactive)
  (if egit-bisect-mode
      (message "Already in bisect mode")
    (dolist (c egit-commits)
      (setf (egit--commit-bisect-state c) t))
    (setq egit-bisect-mode t)
    (egit-update-ewoc)))

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
  (setq egit-highlight-regex (if (string= regex "") nil regex))
  (egit-update-ewoc))

(unless nil ; egit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
      (define-key map "\C-n" 'egit-next-line)
      (define-key map "\C-p" 'egit-previous-line)
      (define-key map "n" 'egit-next-line)
      (define-key map "p" 'egit-previous-line)
      (define-key map "f" 'egit-show-files-commit)
      (define-key map "c" 'egit-cherry-pick)
      (define-key map "R" 'egit-revert)
      (define-key map "g" 'egit-refresh)
      (define-key map "d" 'egit-show-diff-commit)
      (define-key map "q" 'egit-quit)
      (define-key map "m" 'egit-mark)
      (define-key map "u" 'egit-unmark)
      (define-key map "o" 'egit-occur)
      (define-key map "D" 'egit-show-commit-date)
      (define-key map "A" 'egit-show-commit-author)
      (define-key map "i" 'egit-show-commit-id)
      (define-key map "t" 'egit-tag)
      (define-key map "T" 'egit-delete-tag)
      (define-key map "v" 'egit-checkout-file-from-history)
      (define-key map "b" 'egit-show-commit-branch-points)
      (define-key map "Bg" 'egit-bisect-good)
      (define-key map "Bb" 'egit-bisect-bad)
      (define-key map "Bs" 'egit-bisect-start)
      (define-key map "Bq" 'egit-bisect-reset)
      (define-key map "Br" 'egit-bisect-reset)
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
      ["Display Date+Time"  egit-show-commit-time [:selected '(lambda () egit-show-time)]]
      ["Display Author"  egit-show-commit-author [:selected '(lambda () egit-show-author)]]
      ["Display SHA1"    egit-show-commit-id [:selected '(lambda () egit-show-id)]]
      ["Display Branch Points" egit-show-commit-branch-points [:selected '(lambda () egit-show-branch-points)]]
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
      ("Bisect"
       ["Start" egit-bisect-start]
       ["Mark Good" egit-bisect-good]
       ["Mark Bad" egit-bisect-bad]
       ["Reset/Quit" egit-bisect-reset])
      "--------"
      ["Visit Historical File"  egit-checkout-file-from-history t]
      "--------"
      ["Mark"            egit-mark t]
      ["Unmark"          egit-unmark t]
      ["Search/Highlight"   egit-occur t]
      "--------"
      ["Refresh"         egit-refresh t]
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

(defun egit-get-more-commits ()
  (interactive)
  (let ((saved-node-num egit-max-commits))
    (setq egit-max-commits (* egit-max-commits 2))
    (egit-refresh)
    (ewoc-goto-node egit-ewoc (ewoc-nth egit-ewoc saved-node-num))
    (egit-current-line-decoration)
    (egit-show-commit nil)))

(defun egit-mode (commits ref dir n &optional file)
  "Mode for git commit logs.

\\{egit-mode-map}
"
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
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
  (make-local-variable 'egit-show-time)
  (make-local-variable 'egit-show-author)
  (make-local-variable 'egit-show-id)
  (make-local-variable 'egit-show-branch-points)
  (make-local-variable 'egit-show-merge-commits)
  (make-local-variable 'egit-branch-points)
  (make-local-variable 'egit-bisect-mode)
  (make-local-variable 'egit-bisect-good-commit)
  (make-local-variable 'egit-bisect-bad-commit)
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
        egit-show-date t
        egit-show-time nil
        egit-show-author t
        egit-show-id nil
        egit-show-merge-commits t
        egit-show-branch-points nil
        egit-branch-points nil
        egit-bisect-mode nil
        egit-bisect-good-commit nil
        egit-bisect-bad-commit nil
        egit-max-subject-length (egit-largest-commit-subject commits)
        egit-ewoc (ewoc-create 'egit-pretty-printer "" "" t)
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
                         "\n"
                         )
                 (if (and egit-max-commits (= ncommits egit-max-commits))
                     (let ((map (make-sparse-keymap)))
                       (define-key map [mouse-1] 'egit-get-more-commits)
                       (define-key map [mouse-2] 'egit-get-more-commits)
                       (define-key map "" 'egit-get-more-commits)
                       (format (concat "\n" (propertize " Showing %d Commits, Click For More " 
                                                        'face 'egit-more-face
                                                        'keymap map
                                                        'mouse-face 'egit-more-mouse-face))
                               egit-max-commits))
                   (egit-heading (format "\nShowing All %d Commits" ncommits)))
                 )
    )
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
    (if buffer-file-name
        (let ((dir (file-name-directory buffer-file-name)))
          (read-directory-name
           "Egit log of directory: " 
           dir dir t nil))
      (read-directory-name
       "Egit log of directory: " nil nil t nil))))
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
N commits shown. A prefix argument will query for all parameters otherwise
  the default directory, branch, and unlimited commits will be displayed."
  (interactive
   (with-temp-buffer
     (if (not current-prefix-arg)
         (list default-directory
               (car (egit-get-branches))
               egit-default-max-commits)
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
        (read-number "Max number of commits to show or 0 for all " 0)))))
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
