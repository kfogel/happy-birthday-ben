;;;; BMCS Long Tweet: Turn some text into a series of tweet-sized chunks.
;;; 
;;; Copyright (C) 2020 Ben Collins-Sussman
;;; 
;;; (Written by Karl Fogel for Ben's birthday.)
;;;
;;; This software is released under the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; =============
;;; Documentation
;;; =============
;;;
;;; Let's start with the motivation:
;;;
;;;   | From: Ben Collins-Sussman
;;;   | Subject: emacs long-tweet mode?
;;;   | To: Karl Fogel, Red Bean Notd
;;;   | Date: Thu, 2 Jan 2020 14:45:16 -0600
;;;   | 
;;;   | Just for fun, anyone want to write a new emacs mode?
;;;   | 
;;;   | I'd love a way to compose a long story (meant for twitter) in
;;;   | 280-character chunks.  I imagine emacs giving me the appropriate
;;;   | structure & automation to make this easy.
;;;   | 
;;;   | E.g.  I start typing, and the status bar shows me how I have (280-N)
;;;   | characters left with each Nth keystroke, and once I hit 280, it draws
;;;   | an <hr/> or something and puts me into a new tweet-paragraph a couple
;;;   | lines further down the buffer.  But then it probably does something
;;;   | 'smart' too -- like word wrap from the 1st to the 2nd tweet, or maybe
;;;   | even just move the last incomplete sentence from the end of tweet #1
;;;   | to the start of tweet #2.  Oh, and it would automatically add (3/17)
;;;   | notation to the end of each tweet as well.
;;;   | 
;;;   | Any takers?  :-)
;;;
;;; There.  Now you know why this exists.  The way it actually works
;;; is more or less as described above, except that you first compose
;;; the whole narrative (just regular text editing, no special magicky
;;; stuff going on), and after you've got your first draft done, run
;;;
;;;    M-x bmcs-lt-tweetdiv-buffer
;;;
;;; The text is now divided into tweet-sized chunks, separated by --
;;; wait for it -- separators.  The separator preceding each chunk
;;; displays that chunk's length, and if you manually edit a given
;;; chunk the corresponding length indicator updates in real time.
;;; 
;;; If there are multiple chunks, each chunk is given a count tag at
;;; the end, of the form " (N/M)" where N is this chunk and M is the
;;; total number of chunks.  The length of the count tag is included
;;; when calculating the chunk's length, of course.
;;;
;;; (This package doesn't actually post to Twitter for you, nor does
;;; it read from Twitter -- it doesn't know anything about the Twitter
;;; APIs.  Maybe some day it will grow some of that functionality.  In
;;; the meantime, there are other Emacs packages that do such things:
;;; twmode.sourceforge.net, www.emacswiki.org/emacs/twit.el, etc.)
;;; 
;;; =================================
;;; Support, Enhancement, Improvement
;;; =================================
;;;
;;; This package comes with a 1 Neptune SLA, meaning that support,
;;; customizations, and enhancements are provided for a period of one
;;; orbit of Neptune (164 of your Earth years and 322 of your Earth
;;; days) from the time of initial delivery.  Please note that our
;;; popular 1 Pluto SLA is no longer available, although existing 1
;;; Pluto SLAs will continue to be honored.
;;;
;;; Translation into industry-speak:
;;; 
;;; There is no usability without usage.  The only way this thing
;;; becomes good is if you report bugs and I fix them, in a process
;;; that used to be known as "evolution" but is now called "iteration"
;;; because it's easier to get Series B and C funding if one avoids
;;; words commonly associated with processes that take millions of
;;; [your Earth] years.

(defvar bmcs-lt-tweet-length 280
  "*Maximum length of a tweet, in characters.")

(defconst bmcs-lt-consumption-indicator
  (format "↓ [[[ 0 / %d ]]]" bmcs-lt-tweet-length)
  "Template for the real-time progress marker.
Don't change this; some regular expressions depend on it.")

(defconst bmcs-lt-consumption-indicator-re
  "\\(↓\\|↑\\) \\[\\[\\[ \\([0-9]+\\) / \\([0-9]+\\) \\]\\]\\]"
  "A regular expression matching the tweet-length consumption indicator.
The first match group is the numerator; the second is the denominator.")

(defvar bmcs-lt-internal-divider
  (format "\n\n<!-- %s -->\n\n" bmcs-lt-consumption-indicator)
  "*What to insert in the buffer between tweets.
You can change this a bit, but leave the \"%s\" and the two
newlines at the end; there is code that depends on these.  Yes,
by the way, you are right to deduce that if you write text that
looks too much like this marker, things might break.  If that's
ever a problem in real life, please take advantage of your SLA.")

(defun bmcs-lt-boundaries-of-tweet-at-point ()
  "Return a list containing the start and end of the current tweet, or nil.
If not in a tweet, always return nil.  Never throw an error, for the same
reasons as `bmcs-lt-update-tweet-length-indicator', which see."
  (let ((start nil))
    (save-excursion
      (save-match-data
        (when (re-search-backward bmcs-lt-consumption-indicator-re nil t)
          (end-of-line)
          (re-search-forward "[^\n]" nil t)
          (re-search-forward "\\S-" nil t)
          (beginning-of-line)
          (setq start (point))
          (when (re-search-forward bmcs-lt-consumption-indicator-re nil t)
            (beginning-of-line)
            (re-search-backward "[^\n]" nil t)
            (end-of-line)
            (re-search-backward "\\S-" nil t)
            (forward-char 1)
            (list start (point))))))))

(defun bmcs-lt-after-change-function (&rest ignored)
  "Update the appropriate tweet-length consumption indicator, if any."
  (unless undo-in-progress
    (bmcs-lt-update-tweet-length-indicator)))

(defun bmcs-lt-update-tweet-length-indicator ()
  "Update the tweet length indicator corresponding to the current tweet.
Never throw an error, because this function is run from within
`after-change-functions', so throwing an error would cause something
to be removed from that hook that we don't want removed."
  (let ((bounds (bmcs-lt-boundaries-of-tweet-at-point)))
    (when bounds
      (let ((tweet-start (car bounds))
            (tweet-end   (cadr bounds)))
        (save-excursion
          (save-match-data
            ;; If we decide to put the tweet length indicator after the
            ;; tweet instead of before, the only code you have to change
            ;; is that `re-search-backward' becomes `re-search-forward'.
            ;; Which means we could make it conditional, perhaps even
            ;; conditional on the directionality of the arrow in
            ;; `bmcs-lt-consumption-indicator'.  On the theory that we
            ;; might do that some day, both arrows are provided for in
            ;; `bmcs-lt-consumption-indicator-re' just in case.
            (re-search-backward bmcs-lt-consumption-indicator-re)
            (let ((numerator-start   (copy-marker (match-beginning 2)))
                  (numerator-end     (copy-marker (match-end 2)))
                  (denominator-start (copy-marker (match-beginning 3)))
                  (denominator-end   (copy-marker (match-end 3))))
              (delete-region numerator-start numerator-end)
              (goto-char numerator-start)
              (insert (number-to-string (- tweet-end tweet-start)))
              (delete-region denominator-start denominator-end)
              (goto-char denominator-start)
              (insert (number-to-string bmcs-lt-tweet-length)))))))))

(define-minor-mode bmcs-lt-mode
  "Minor mode for Ben Collins-Sussman to compose tweets in.
When this mode is active, it updates the appropriate tweet-length
progress marker as you type.  Those markers are usually initially
introduced into the buffer by `bmcs-lt-tweetdiv-region'."
  nil
  " BMCS-LT"
  nil
  (if bmcs-lt-mode
      (add-hook 'after-change-functions 'bmcs-lt-after-change-function)
    (remove-hook 'after-change-functions 'bmcs-lt-after-change-function)))

(defun bmcs-lt-safe-forward-sentence (&optional arg)
  "Whoo-whee.  Let me explain... No, there is too much.  Let me sum up.

This package switches off between `forward-sentence' and `forward-word'
to change the granularity at which we're dividing text into chunks.
It's able to do that switch-off cleanly because the two functions have
the same calling convention.

However, there is a quirk of `forward-sentence' that unspools into a
deep weirdness in Emacs if you look closely enough (which, as it
happens, I have).  While `forward-word' will just return nil if it
can't move any further because point is at the end of the buffer
(let's just deal with the positive-arg case), `forward-sentence'
claims, in its doc string, that it signals an error in that case.

Here the weirdness begins: it *does* signal an error (\"End of
buffer\"), but apparently some errors are more equal than others:
this error won't produce a backtrace when `toggle-debug-on-error'
is on, but you *can* catch it with `condition-case'.  Perhaps there is
more I need to know about how errors work in Emacs.  So I took a look
at the code.

Ultimately, `forward-sentence' behaves this way because it calls
`end-of-paragraph-text' (both are in lisp/textmodes/paragraphs.el in
the Emacs sources) which in turn eventually calls `forward-char'
(which also has that odd semi-error behavior), which is really
Fforward_char() in src/cmds.c, which calls move_point(), which on
end of buffer invokes xsignal0() (src/eval.c), which invokes
xsignal() (inline function defined in src/lisp.h), which invokes
Fsignal() (src/eval.c), which invokes signal_or_quit() (same file)
which is a somewhat long function and at this point I took a deep
breath and realized that if you're not reading this, why am I writing
it?  The point is, I just need `forward-sentence' to return nil
if it can't move forward any more, hence this wrapper."
  (condition-case nil (forward-sentence arg) (t nil)))

(defun bmcs-lt-tweetdiv-region (b e)
  "Divide region from B to E into tweet-sized chunks and enter `bmcs-lt-mode'."
  (interactive "r")
  (when (> b e) ; ensure ordering
    (setq e (+ b e) b (- e b) e (- e b)))
  (setq e (copy-marker e))
  (let* ((func 'bmcs-lt-safe-forward-sentence) ; try moving by sentences first
         (opoint nil) ; start of this tweet
         (hitch nil)  ; latest place from which we did forward-{sentence,word}
         (fuzz 8)    ; room for " (NN/NN)" at the end
         (available-len (- bmcs-lt-tweet-length fuzz))
         (num-tweets 0)
         )
    (bmcs-lt-mode -1)
    (save-excursion
      (goto-char b)
      (insert bmcs-lt-internal-divider)
      (setq opoint (point))
      (while (< (point) e)
        (catch 'end-chunk
          (while (< (- (point) opoint) available-len)
            (let ((spot (point)))
              (funcall func 1)
              ;; Sometimes you reach the end but e is still ahead.
              ;; In that case, just set e and throw, to stop the loop.
              (when (= (point) spot)
                (setq e (point))
                (throw 'end-chunk nil)))
            (when (> (- (point) opoint) available-len)
              ;; Hit reverse, wheels a-squealing.
              (funcall func -1)
              ;; This just handles an edge case: if our very first
              ;; excursion started in the middle of a sentence (or perhaps
              ;; word, though that's even less likely), then moving back
              ;; by that unit would take us back past the origin point.
              ;; This code protects against that.  Even though it's run on
              ;; every tweet, it would only ever have any useful effect in
              ;; the first tweet; the rest of the time, it's nugatory.
              (when (< (point) opoint)
                (goto-char opoint))
              ;; If we're still halfway or more through the available
              ;; length, then just divide here.
              (if (< (- available-len (- (point) opoint))
                     (/ available-len 2))
                  (throw 'end-chunk nil)
                ;; Otherwise, start moving by words... unless we
                ;; already were, in which case we're done here (and
                ;; there's some really long word that we're going to
                ;; have a problem with later).
                (if (eq func 'bmcs-lt-safe-forward-sentence)
                    (setq func 'forward-word)
                  (error "You have a really, really long word there."))))))
        (just-one-space)
        (delete-char -1)
        (let ((spot (point)))
          (insert bmcs-lt-internal-divider)
          (setq num-tweets (1+ num-tweets))
          (save-excursion
            (goto-char spot)
            (fill-paragraph)))
        (setq opoint (point) func 'bmcs-lt-safe-forward-sentence))
      ;; e might still be inside a word, so just set it to wherever we
      ;; are now.
      (setq e (point-marker))
      (goto-char b)
      (re-search-forward bmcs-lt-consumption-indicator-re)
      (let ((this-tweet-count 1))
        (while (and
                (< (point) e)
                (re-search-forward bmcs-lt-consumption-indicator-re nil t))
          (save-excursion
            (beginning-of-line)
            (re-search-backward "\\S-")
            (end-of-line)
            (when (> num-tweets 1)
              (insert (format " (%d/%d)" this-tweet-count num-tweets)))
            (bmcs-lt-update-tweet-length-indicator))
          (setq this-tweet-count (1+ this-tweet-count)))))
    (bmcs-lt-mode 1)))

(defun bmcs-lt-tweetdiv-buffer ()
  "Divide the buffer's text into tweet-sized chunks.
This just calls `bmcs-lt-tweetdiv-region', which see."
  (interactive)
  (bmcs-lt-tweetdiv-region (point-min) (point-max)))
