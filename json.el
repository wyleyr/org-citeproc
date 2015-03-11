;; Helper; shouldn't Elisp have this already??
(defun map-plist (f plist)
  "Map F over the key-value pairs in PLIST.  F is called with
each key and its associated value as its first and second
arguments, respectively."
  (if (or (null plist) (null (cdr plist))) '() ; values must be paired
    (let ((k (car plist))
          (v (cadr plist)))
        (cons (funcall f k v) (map-plist f (cddr plist))))))

(defun org-citation-to-json (citation)
  "Translate an Org citation object to JSON data that can be read
by citeproc-js or citeproc-hs"
  (let* ((parenp (org-element-property :parenthetical citation))
	 (references
	  ; TODO: is this the right way to get the list of references
	  ; within a citation, now that they are a separate object type?
	  (remove-if-not
	   (lambda (o) (and (listp o) (eq (car o) 'citation-reference)))
	   citation))
         (json-refs
	  (mapconcat 'org-citation-reference-to-json references ", ")))
    (if references
        ;; TODO: add properties key that sets noteIndex
	;; TODO: handle common prefix and suffix 
        (format "{ \"citationItems\": [ %s ] }" json-refs)
      "")))

(defun org-citation-reference-to-json (reference)
  "Translate a citation-reference within an Org citation object
to JSON data that can be read by citeproc-js or citeproc-hs"
  (let* ((parenp (org-element-property :parenthetical
				       (org-element-property :parent reference)))
	 (json-props (map-plist 'org-citation--reference-property-to-json
				(append (list :parenthetical parenp)
					(nth 1 reference))))
	 (json-data (remove-if-not 'identity json-props)))
    (if json-data (format "{ %s }" (mapconcat 'identity json-data ", "))
      "")))

(defun org-citation--reference-property-to-json (prop val)
  "Translate a property of a citation-reference to JSON data"
  ; TODO: prefix and suffix vals should be transcoded by the current
  ; export backend
  (case prop
    (:key (format "\"id\": \"%s\"" val))
    (:prefix (format "\"prefix\": \"%s\"" (org-element-interpret-data val)))
    (:suffix (format "\"suffix\": \"%s\"" (org-element-interpret-data val)))
    (:parenthetical (if (not val) (format "\"author-in-text\": true") nil))
    ;; TODO: at least citeproc-hs uses these fields, so we should extract them:
    (:suppress-author (if val (format "\"suppress-author\": true") nil))
    (:author-only (if val (format "\"author-only\": true") nil))
    (:label (format "\"label\": \"%s\"" val))
    (:locator (format "\"locator\": \"%s\"" val))
      ; otherwise, case returns nil
    ))

