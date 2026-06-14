;; Daily notes
;;
;; Creates daily notes with a given default content like daily tasks.
;; Old notes are being archived in a folder structure looking like YYYY/MM/DD.md
;;
;; TODO: reload the UI after note changes (currently not exposed)
(let*
  ([root "/daily/"]
   [today (js-eval "new Intl.DateTimeFormat('en-CA').format(new Date());")]
   [today-file (string-append root today ".md")]
   [default-content
  "# Daily tasks
  - [ ] Foo
  - [ ] Bar
  - [ ] Baz"
   ])
  ;; create a daily note if it doesn't exist
  (if (nil? (opfs-read today-file))
    (opfs-write today-file default-content)
    nil)
  ;; archive previous daily notes
  (for entry in (opfs-list root)
    (if
      (and
        (string-suffix? entry ".md")
        (not (string-prefix? entry today)))
      (if
        (opfs-write
          (string-append root (reduce (fn (x y) (string-append x "/" y)) (string-split entry "-")))
          (opfs-read (string-append root entry)))
        (opfs-delete (string-append root entry))
        nil)
      nil)))

;; vim: set ts=2 sw=2 et :
