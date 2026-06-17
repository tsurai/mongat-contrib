;; Japanese vocabulary lookup and flashcard creation
;;
;; This script utilizes the api of jotoba.de to lookup a given word and display
;; the results. Clicking the add button next to a search result creates a flascard
;; utilizing FSRS in an indexedDB database.
;;
;; Please be mindful of the API and don't use it for mass data processing.

(let
  ([db-name "core"]
   [store-name "vocabs"]
   [db (idxdb-open 1 db-name '((store-name ("due" #f #f "due") ("data" #t #f "data"))))]
   [idx (gen-seq)])
  (defun lookup (query)
    (set! lookup-results
      (cdr-assoc "words"
        (json->list
          (fetch
            "https://jotoba.de/api/search/words"
            "POST"
            '(("Content-Type" . "application/json"))
            (string-append
              "{ \"query\":\""
              query
              "\"}")))))
  (print
    (reduce string-append
      (map (fn (result)
        (let
          ([kanji (cdr-assoc '("reading" "kanji") result)]
           [kana (cdr-assoc '("reading" "kana") result)]
           [meaning (cdr-assoc "senses" result)])
          (string-append
            kanji
            " (" kana ")<br>"
            (string-join
              (map
                (fn (x)
                  (reduce
                    (fn (x y)
                      (if (nil? y) x (string-append x ", " y)))
                    (cdr-assoc "glosses" x)))
                meaning)
              "<br>")
            "<br><button onclick='window.exec_gat(&quot;(add-vocab \\"\" + document.getElementById(&quot;source&quot;).value + \"\\" "
            (idx)
            ")&quot;)'>add</button>"
          "<br><br>" )))
      lookup-results)))
  (defun add-vocab (source idx)
    (idxdb-insert db "vocabs" #f (list->json (list '("s" . 0) '("d" . 0) '("t" . 0) (cons "due" (date)) (cons "source" source) (cons "data" (list->json (vector-get lookup-results idx))))))))

;; Frontend side - put this below the gat code in the note
;;
;; <style>
;; @scope {
;;   * {
;;     padding: 5px;
;;     margin: 0 5px;
;;   }
;;   input {
;;     color: var(--clr-bg);
;;   }
;; }
;; </style>
;; <select id='source'>
;;   <option>A</option>
;;   <option>B</option>
;;   <option>C</option>
;; </select>
;; <input id="query" type="text"></input>
;; <button onclick='window.exec\_gat("(lookup \"" + document.getElementById("query").value + "\")", "results")'>query</button>
;;
;; <code id="results"></code>

;; vim: set ts=2 sw=2 et :
