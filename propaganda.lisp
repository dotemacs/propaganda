;; Lisp propaganda
(defpackage :propaganda
  (:use :cl)
  (:local-nicknames (#:http :dex))
  (:local-nicknames (#:feeder :org.shirakumo.feeder))
  (:import-from :uiop #:getenv)
  (:import-from :local-time #:timestamp< #:universal-to-timestamp)
  (:import-from :cl-ppcre #:scan)
  (:import-from :arrows #:-> #:->>)
  (:export #:main))

(in-package :propaganda)

(defparameter *config*
  (list :masto-endpoint "https://functional.cafe/api/v1/statuses"
        :access-token (getenv "MASTODON_ACCESS_TOKEN")
        :db-file (or (getenv "DATABASE_PATH") "propaganda.db")
        :feeds '("http://abcl-dev.blogspot.com/atom.xml"
                 ;; Planet Lisp uses "https://journal.paoloamoroso.com/tag:CommonLisp/feed/"
                 "https://journal.paoloamoroso.com/tag:Lisp/feed/"
                 "http://within-parens.blogspot.com/atom.xml"
                 "https://lispnews.wordpress.com/feed/atom/"
                 "https://40ants.com/lisp-project-of-the-day/rss.xml"
                 "https://lispblog.xach.com/rss"
                 "https://borretti.me/feed.xml"
                 "https://www.tfeb.org/fragments/feeds/lisp.rss.xml"
                 "https://scottlburson2.blogspot.com/atom.xml"
                 "http://cl-test-grid.blogspot.com/feeds/posts/default"
                 "http://common-lisp-net.tumblr.com/rss"
                 "http://p-cos.blogspot.com/atom.xml"
                 "https://drmeister.wordpress.com/feed/"
                 "http://lisp-univ-etc.blogspot.com/feeds/posts/default/-/lisp/en"
                 "https://common-lisp.net/project/ecl/rss.xml"
                 "https://www.fitzsim.org/blog/?feed=rss2&cat=6"
                 "http://michaeljforster.tumblr.com/tagged/lisp/rss"
                 "https://tychoish.com/tags/lisp/index.xml"
                 "https://experimentalprogramming.wordpress.com/feed/"
                 "https://lisper.in/feed/lisp.xml"
                 "https://blog.tymoon.eu/api/reader/atom?tag=common%20lisp"
                 "http://lhealy.livejournal.com/data/atom"
                 "https://mov.im/?feed/phoe%40movim.eu"
                 "https://netzhansa.blogspot.com/feeds/posts/default"
                 "https://enthusiasm.cozy.org/archives/category/common-lisp/feed/atom"
                 "https://blog.cddr.org/tags/lisp/index.xml"
                 "http://johnj.com/tags/lisp/index.xml"
                 "https://medium.com/feed/@kaveh808"
                 "http://www.pvk.ca/atom.xml"
                 "https://turtleware.eu/rss.xml"
                 "http://enlivend.livejournal.com/data/atom"
                 "https://lispjobs.wordpress.com/feed/"
                 "https://cjelupton.wordpress.com/feed/"
                 "http://funcall.blogspot.com/atom.xml"
                 "https://www.n16f.net/tags/lisp/index.xml"
                 "https://common-lisp.net/project/mcclim/rss.xml"
                 "http://quotenil.com/lisp.rss"
                 "https://nmunro.github.io/feed.xml"
                 "https://www.darkchestnut.com/feed.xml"
                 "http://kvardek-du.kerno.org/feeds/posts/default/-/lisp"
                 "http://tkpapp.blogspot.com/atom.xml"
                 "http://blog.quicklisp.org/atom.xml"
                 "http://blog.matroid.org/archives.rss?Lisp"
                 "http://christophe.rhodes.io/notes/blog/feeds/lisp/index.atom"
                 "https://fare.livejournal.com/data/atom"
                 "http://trittweiler.blogspot.com/atom.xml"
                 "https://readevalprint.tumblr.com/rss"
                 "http://random-state.net/rss.xml"
                 "http://slime-tips.tumblr.com/rss"
                 "http://nklein.com/feed/"
                 "https://jorgetavares.com/feed/"
                 "https://www.timmons.dev/tag/common-lisp-rss.xml"
                 "https://terranostra.one/rss.xml"
                 "https://didierverna.net/tags/lisp/index.xml"
                 "https://lisp-journey.gitlab.io/index.xml"
                 "http://blog.funcall.org/feed.xml"
                 "https://mstmetent.blogspot.com/feeds/posts/default"
                 "http://langnostic.inaimathi.ca/feed/atom/by-tag/common-lisp")))

(defun load-feeds-db ()
  "Load feeds data from local file."
  (let ((db-file (getf *config* :db-file)))
    (if (probe-file db-file)
        (with-open-file (stream db-file :direction :input)
          (read stream))
        '())))

(defun save-feeds-db (data)
  "Save feeds data to local file."
  (let ((db-file (getf *config* :db-file)))
    (with-open-file (stream db-file :direction :output :if-exists :supersede)
      (prin1 data stream))
    data))

(defun get-feed-info (url)
  "Get latest post date for a specific feed URL."
  (cdr (assoc url (load-feeds-db) :test #'string=)))

(defun update-feed-info (url latest-post-date)
  "Update latest post date for a specific feed."
  (let* ((feeds (load-feeds-db))
         (existing (assoc url feeds :test #'string=)))
    (if existing
        (setf (cdr existing) latest-post-date)
        (push (cons url latest-post-date) feeds))
    (save-feeds-db feeds)))

(defun detect-feed-format (feed)
  "Detect if the `feed' is RSS or Atom.
Returns two values: parsed-feed and format ('atom or 'rss), or NIL NIL
on failure"
  (handler-case
      (let ((parsed-feed (feeder:parse-feed feed :rss)))
        (values parsed-feed 'rss))
    (error ()
      (handler-case
          (let ((parsed-feed (feeder:parse-feed feed :atom)))
            (values parsed-feed 'atom))
        (error ()
          (values nil nil))))))

;; clean the URLs

(defun split-string (string delimiter)
  (loop with start = 0
        with end = (position delimiter string :start start)
        collect (subseq string start end)
        while end
        do (setf start (1+ end)
                 end (position delimiter string :start start))))

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun sanitize-url (url-string)
  "Remove utm_* from the URL, as well as add a prefix of https:// if
omitted."
  (let ((url-with-scheme (if (or (string-prefix-p "http://" url-string)
                                 (string-prefix-p "https://" url-string))
                             url-string
                             (concatenate 'string "https://" url-string))))
    (let* ((uri (puri:parse-uri url-with-scheme))
           (query (puri:uri-query uri)))
      (when query
        (let ((clean-params
                (remove-if (lambda (param)
                             (let ((key (car (split-string param "="))))
                               (string-prefix-p "utm_" key)))
                           (split-string query "&"))))
          (setf (puri:uri-query uri)
                (if clean-params
                    (format nil "~{~A~^&~}" clean-params)
                    nil))))
      (puri:render-uri uri nil))))


;; tooting

(defun post-toot (toot)
  (let ((access-token (concatenate 'string "Bearer " (getf *config* :access-token))))
    (multiple-value-bind (body status-code headers uri stream)
        (http:post (getf *config* :masto-endpoint)
                   :headers `(("Authorization" . ,access-token))
                   :content `(("status" . ,toot)
                              ("visibility" . "public")))
      (declare (ignore headers uri stream))
      (if (= status-code 200)
          (format t "Success! Response: ~A~%" body)
          (format t "Failed with status: ~A~%" status-code)))))

(defun format-toot (title link)
  "Formats the `toot' & `link' for tooting"
  (let ((double-newline (make-string 2 :initial-element #\Newline)))
    (concatenate 'string title
                 double-newline
                 link
                 double-newline
                 "#lisp")))

;; check the content

(defun html-body-to-text (url)
  (multiple-value-bind (body status headers)
      (http:get url)
    (declare (ignore headers))
    (when (= status 200)
      (let* ((doc (plump:parse body))
             (html-body (plump:get-elements-by-tag-name doc "body")))
        (when (and html-body (> (length html-body) 0))
          (plump:text (elt html-body 0)))))))

(defun lisp-content-p (content)
  (search "lisp" (string-downcase content)))

;; clean up fractional seconds

(defun strip-fractional-seconds (xml)
  "Strips fractional seconds in timestamps:

2025-09-01T18:38:47.117-07:00 -> 2025-09-01T18:38:47-07:00

(NOTE: that `.117' is stripped away)

Useful for feeds that don't follow RFC3339, like those generated by
Google's Blogger."
  (cl-ppcre:regex-replace-all
   "([Tt]\\d\\d:\\d\\d:\\d\\d)\\.\\d+(Z|[+-]\\d\\d:\\d\\d)"
   xml "\\1\\2"))

;; process all

(defun check-all-feeds-for-updates ()
  "Check all feeds for newer posts than what's recorded in the db."
  (let ((feeds (load-feeds-db)))
    (dolist (feed feeds)
      (let ((url (car feed))
            (stored-date (cdr feed)))
        (format t "processing: ~A~%" url)
        (handler-case
            (let* ((response (http:get url))
                   (parsed-feed (handler-bind ((warning #'muffle-warning))
                                  (-> response
                                      strip-fractional-seconds
                                      (feeder:parse-feed t))))
                   (all-entries (-> parsed-feed first feeder:content))
                   (newest-date stored-date)
                   (found-new-posts nil))
              (dolist (entry all-entries)
                (let* ((entry-date (feeder:published-on entry))
                       (entry-link (feeder:link entry))
                       (entry-title (feeder:title entry))
                       (entry-title-read (if (stringp entry-title)
                                             entry-title (plump:text entry-title)))
                       (date-string (local-time:format-timestring nil entry-date))
                       (url (-> entry-link feeder:url sanitize-url))
                       (lisp-content (-> url html-body-to-text lisp-content-p)))
                  (when (and lisp-content
                             (or (null stored-date)
                                 (string> date-string stored-date)))
                    (setf found-new-posts t)
                    (format t "New post found in feed ~A~%" url)
                    (format t "  Lisp content: ~A~%" lisp-content)
                    (format t "  Post title: ~A~%" entry-title-read)
                    (format t "  Post date: ~A~%" date-string)
                    (format t "  Post URL: ~A~%" url)
                    #+nil (format t "  Toot token: ~A~%" (getf *config* :access-token))
                    (format t "  toot: ~A~%" (format-toot entry-title-read url))
                    (format t "~%")
                    (when (or (null newest-date)
                              (string> date-string newest-date))
                      (setf newest-date date-string)))))
              (when found-new-posts
                (update-feed-info url newest-date)))
          (error (e)
            #+nil (format t "Error checking feed ~A: ~A~%" url e)))))))

#+nil (defun reset-db ()
        "Helper function to, well, reset the db."
        (dolist (url (getf *config* :feeds))
          (update-feed-info url "2025-08-21T00:00Z")))

(defun main ()
  "Main entry point for the propaganda executable."
  (check-all-feeds-for-updates))
