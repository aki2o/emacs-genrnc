(eval-when-compile (require 'cl))
(require 'rx)
(require 'regexp-opt)
(require 'rng-loc)
(require 'em-glob)
(require 'url)
(require 'deferred)
(require 'concurrent)
(require 'log4e)
(require 'yaxception)


(defgroup genrnc nil
  "Generate RELAX NG schema (using the compact syntax) from XML schema, RELAX NG schema and DTD."
  :group 'nxml
  :prefix "genrnc-")

(defcustom genrnc-user-schemas-directory "~/.emacs.d/schema"
  "Directory of storing external scheme and \"schemas.xml\".
\"schemas.xml\" is XML file defining external schema.
This file added `rng-schema-locating-files' for nXML-mode."
  :type 'directory
  :group 'genrnc)

(defcustom genrnc-download-timeout 120000
  "Time of waiting downloading schema."
  :type 'integer
  :group 'genrnc)


(log4e:deflogger "genrnc"
                 "%t [%l] %m"
                 "%H:%M:%S"
                 '((fatal . "fatal")
                   (error . "error")
                   (warn  . "warn")
                   (info  . "info")
                   (debug . "debug")
                   (trace . "trace")))
(genrnc--log-set-level 'trace)


(defstruct genrnc--schema name state errmsg)

(defvar genrnc--index-count 0)
(defvar genrnc--own-directory nil)
(defvar genrnc--defined-typeid-list nil)
(defvar genrnc--hash-namespace-files (make-hash-table :test 'equal))
(defvar genrnc--hash-schema-cache (make-hash-table :test 'equal))
(defvar genrnc--schemas-file-name "schemas.xml")


(defun genrnc-regist-url (url)
  "Regist schema by downloading from given URL.

Specification:
 - Accept the following schema type.
   - XML schema (xsd)
   - RELAX NG schema (rng)
   - RELAX NG Compact schema (rnc)
   - DTD schema (dtd)
 - If the schema isn't rnc, convert it to rnc.
 - If the schema include or import other schema, regist them not displaying for you.
 - If the schema is xsd and import abbreviated schemaLocation (ex. <import namespace=\"...\"/> ), run the following actiona.
   - If the namespace found in already registed schema, import them.
   - Else, prompt for you to identify its location in minibuffer.
 - The registed schema be stored in `genrnc-user-schemas-directory'.

When Error:
 - "
  (interactive "s[GENRNC] Schema URL: ")
  (yaxception:$
    (yaxception:try
      (if (not (string-match "^https?://" url))
          (message "[GENRNC] '%s' is invalid URL" url)
        (genrnc--info "start regist url : %s" url)
        (genrnc--load-index-count)
        (let* ((fext (genrnc--get-schema-type url)))
          (genrnc--update-namespace-files)
          (genrnc--generate url fext))))
    (yaxception:catch 'error e
      ;; (genrnc--debug "" (yaxception:get-stack-trace-string e))
      (genrnc--finish-generate url 'failed t (yaxception:get-text e)))))

(defun genrnc-regist-file (path)
  "Regist schema by copying from given PATH.

Specification:
 - Accept the following schema type.
   - XML schema (xsd)
   - RELAX NG schema (rng)
   - RELAX NG Compact schema (rnc)
   - DTD schema (dtd)
 - If not rnc given, convert it to rnc.
 - If the schema including or importing other schema, regist them not displaying for you.
 - If the schema is xsd and including import abbreviated schemaLocation (ex. <import namespace=\"...\"/> ), run action the following.
   - If the namespace found in already registed schema, import them.
   - Else, prompt you for to identify its location in minibuffer.
 - The registed schema be stored in `genrnc-user-schemas-directory'."
  (interactive "f[GENRNC] Schema File Path: ")
  (yaxception:$
    (yaxception:try
      (genrnc--info "start regist path : %s" (expand-file-name path))
      (genrnc--load-index-count)
      (let* ((fext (genrnc--get-schema-type path)))
        (genrnc--update-namespace-files)
        (genrnc--generate (expand-file-name path) fext)))
    (yaxception:catch 'error e
      (genrnc--finish-generate (expand-file-name path) 'failed t (yaxception:get-text e)))))

(defun genrnc-update-user-schema (typeid)
  "Update schema by selection from the registed typeid list.

Specification:
 - If the schema including or importing other schema, update them not displaying for you.
 - If the schema is xsd and including import abbreviated schemaLocation (ex. <import namespace=\"...\"/> ), not update it."
  (interactive
   (list (completing-read "[GENRNC] Select Schema Type ID: " (genrnc--get-schemas-typeid-list-by-user) nil t)))
  (yaxception:$
    (yaxception:try
      (genrnc--info "start update schema of '%s'" typeid)
      (genrnc--init-schema-directory)
      (let* ((idx (genrnc--get-index-from-typeid typeid))
             (location (genrnc--get-schema-location idx))
             (fext (genrnc--get-schema-type location)))
        (yaxception:$
          (yaxception:try
            (genrnc--update-namespace-files)
            (genrnc--generate location fext nil t))
          (yaxception:catch 'error e
            (genrnc--finish-generate location 'failed t (yaxception:get-text e) nil t)))))
    (yaxception:catch 'error e
      (genrnc--error "failed update schema of '%s' : %s" typeid (yaxception:get-text e))
      (message "[GENRNC] Failed update schema of '%s' : %s" typeid (yaxception:get-text e)))))

(defun genrnc-delete-user-schema (typeid)
  "Delete schema file and definition in \"schemas.xml\" by selection from the registed typeid list."
  (interactive
   (list (completing-read "[GENRNC] Select Schema Type ID: " (genrnc--get-schemas-typeid-list-by-user) nil t)))
  (if (not (y-or-n-p (format "Delete schema of '%s'? " typeid)))
      (message "[GENRNC] Not deleted schema of '%s'." typeid)
    (yaxception:$
      (yaxception:try
        (genrnc--init-schema-directory)
        (let* ((idx (genrnc--get-index-from-typeid typeid)))
          (genrnc--remove-schema idx t)
          (setq genrnc--defined-typeid-list nil)
          (setq genrnc--hash-namespace-files (make-hash-table :test 'equal))
          (setq genrnc--hash-schema-cache (make-hash-table :test 'equal))
          (message "[GENRNC] Finished delete schema of '%s'." typeid)))
      (yaxception:catch 'error e
        (genrnc--error "failed delete schema of '%s' : %s" typeid (yaxception:get-text e))
        (message "[GENRNC] Failed delete schema of '%s' : %s" typeid (yaxception:get-text e))))))

(defun genrnc-rename-user-schema (typeid)
  "Change schema typeid that is identifier when select applicable schema on nXML-mode."
  (interactive
   (list (completing-read "[GENRNC] Select Schema Type ID: " (genrnc--get-schemas-typeid-list-by-user) nil t)))
  (genrnc--init-schema-directory)
  (let* ((idx (genrnc--get-index-from-typeid typeid))
         (location (genrnc--get-schema-location idx))
         (newtypeid (genrnc--read-typeid location typeid)))
    (genrnc--remove-schemas-defining typeid)
    (genrnc--store-schemas-defining idx newtypeid)
    (message "[GENRNC] Finished rename schema.")))

(defun genrnc-clear-cache ()
  "Delete the cached information of the execution environment and the user input when you run the schema registration and renewal."
  (interactive)
  (if (not (y-or-n-p "Clear working cache? "))
      (message "[GENRNC] Not clear working cache.")
    (setq genrnc--index-count 0)
    (setq genrnc--own-directory nil)
    (setq genrnc--defined-typeid-list nil)
    (setq genrnc--hash-namespace-files (make-hash-table :test 'equal))
    (setq genrnc--hash-schema-cache (make-hash-table :test 'equal))
    (message "[GENRNC] Finished clear working cache.")))


(defvar genrnc--regexp-index-line (rx-to-string `(and bol (group (+ numeric)) (+ space) (group (+ not-newline)) eol)))
(defun genrnc--load-index-count ()
  (genrnc--trace "start load index count")
  (genrnc--init-schema-directory)
  (when (= genrnc--index-count 0)
    (with-current-buffer (genrnc--get-index-file-buffer)
      (setq genrnc--index-count (loop with maxidx = 0
                                      initially (goto-char (point-min))
                                      for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
                                      until (eobp)
                                      when (string-match genrnc--regexp-index-line line)
                                      do (let* ((curridx (string-to-number (match-string-no-properties 1 line))))
                                           (when (> curridx maxidx)
                                             (setq maxidx curridx)))
                                      finally return maxidx
                                      do (forward-line 1)))))
  (genrnc--trace "found index count [ %s ]" genrnc--index-count))

(defvar genrnc--trang-jar-name "trang.jar")
(defvar genrnc--xsd2rng-jar-name "xsd2rng.jar")
(defvar genrnc--xsd2rng-xsl-name "xsd2rng.xsl")
(defun genrnc--init-schema-directory (&optional force)
  (when (and force
             (file-directory-p (genrnc--get-absolute-user-path)))
    (genrnc--info "delete file in [ %s ]" (genrnc--get-absolute-user-path))
    (loop for filenm in (directory-files (genrnc--get-absolute-user-path))
          for f = (genrnc--get-absolute-user-path filenm)
          when (file-regular-p f)
          do (progn (delete-file f)
                    (genrnc--trace "deleted '%s'" f))))
  (when (not (file-directory-p (genrnc--get-absolute-user-path)))
    (genrnc--trace "mkdir [ %s ]" (genrnc--get-absolute-user-path))
    (make-directory (genrnc--get-absolute-user-path)))
  (genrnc--get-schemas-file-buffer)
  (add-to-list 'rng-schema-locating-files (genrnc--get-absolute-user-path genrnc--schemas-file-name))
  (loop for f in (list genrnc--trang-jar-name genrnc--xsd2rng-jar-name genrnc--xsd2rng-xsl-name)
        for orgpath = (concat (genrnc--get-own-directory) "/" f)
        for userpath = (genrnc--get-absolute-user-path f)
        if (and (file-exists-p orgpath)
                (not (file-exists-p userpath)))
        do (copy-file orgpath userpath)))

(defun* genrnc--get-absolute-user-path (&rest filenm)
  (let* ((filenmtext (apply 'concat filenm)))
    (cond ((or (not (stringp filenmtext))
               (string= filenmtext ""))
           (expand-file-name genrnc-user-schemas-directory))
          (t
           (concat (expand-file-name genrnc-user-schemas-directory) "/" filenmtext)))))

(defun genrnc--get-own-directory ()
  (or genrnc--own-directory
      (setq genrnc--own-directory
            (loop for dir in load-path
                  if (directory-files dir nil "\\`genrnc\\.el\\'")
                  return (setq genrnc--own-directory dir)))))

(defvar genrnc--index-file-name "index.txt")
(defvar genrnc--index-file-buffer-name " *genrnc index*")
(defun genrnc--get-index-file-buffer ()
  (or (get-buffer genrnc--index-file-buffer-name)
      (let* ((idxfile (genrnc--get-absolute-user-path genrnc--index-file-name))
             (idxexist (file-exists-p idxfile))
             (idxbuff (find-file-noselect idxfile)))
        (with-current-buffer idxbuff
          (when (not idxexist)
            (genrnc--info "not yet exist index file [ %s ]" idxfile)
            (insert "\n")
            (save-buffer))
          (rename-buffer genrnc--index-file-buffer-name))
        idxbuff)))

(defvar genrnc--schemas-file-buffer-name " *genrnc schemas*")
(defun genrnc--get-schemas-file-buffer ()
  (or (get-buffer genrnc--schemas-file-buffer-name)
      (let* ((sfile (genrnc--get-absolute-user-path genrnc--schemas-file-name))
             (sexist (file-exists-p sfile))
             (sbuff (find-file-noselect sfile)))
        (with-current-buffer sbuff
          (when (not sexist)
            (genrnc--info "not yet exist schemas file [ %s ]" sfile)
            (insert "<locatingRules xmlns=\"http://thaiopensource.com/ns/locating-rules/1.0\">\n")
            (insert "</locatingRules>\n")
            (save-buffer))
          (rename-buffer genrnc--schemas-file-buffer-name))
        sbuff)))

(defvar genrnc--regexp-schema-type (rx-to-string `(and "." (group (+ (not (any "." "/" "\\")))) eos)))
(defun genrnc--get-schema-type (url_or_path)
  (genrnc--trace "start get scema type : %s" url_or_path)
  (let* ((fext (when (string-match genrnc--regexp-schema-type url_or_path)
                 (match-string-no-properties 1 url_or_path)))
         (fext (downcase fext)))
    (genrnc--trace "got part of extention '%s' of '%s'" fext url_or_path)
    (cond ((member fext '("xsd" "rng" "dtd" "rnc")) fext)
          ((member fext '("mod" "ent")) "dtd")
          (t (completing-read (format "[GENRNC] Select schema type of '%s' (xsd|rng|rnc|dtd): " url_or_path)
                              '("xsd" "rng" "rnc" "dtd")
                              nil
                              t)))))

(defun genrnc--update-namespace-files ()
  (genrnc--trace "start update namespace files")
  (let* ((nshash (make-hash-table :test 'equal)))
    (loop for f in rng-schema-locating-files
          do (loop for rule in (rng-get-parsed-schema-locating-file f)
                   if (eq (car rule) 'namespace)
                   do (let* ((idcons (assq 'typeId (cdr rule)))
                             (nscons (assq 'ns (cdr rule)))
                             (id (cdr idcons))
                             (ns (cdr nscons)))
                        (puthash id ns nshash))))
    (loop for f in rng-schema-locating-files
          do (loop with dirpath = (file-name-directory f)
                   for rule in (rng-get-parsed-schema-locating-file f)
                   if (eq (car rule) 'typeId)
                   do (let* ((idcons (assq 'id (cdr rule)))
                             (uricons (assq 'uri (cdr rule)))
                             (id (cdr idcons))
                             (filepath (replace-regexp-in-string "^file:///" "" (cdr uricons)))
                             (ns (gethash id nshash))
                             (nsfiles (when ns
                                        (gethash ns genrnc--hash-namespace-files))))
                        (genrnc--trace "found entry namespace:[%s] file:[%s]" ns filepath)
                        (when (and ns
                                   (file-exists-p filepath)
                                   (not (member filepath nsfiles)))
                          (genrnc--trace "add namespace file:[%s]" filepath)
                          (add-to-list 'nsfiles filepath)
                          (puthash ns nsfiles genrnc--hash-namespace-files)))))))

(defvar genrnc--dfenv (cc:dataflow-environment))
(defun genrnc--generate (url_or_path fext &optional typeid force)
  (genrnc--trace "start generate '%s'. fext:[%s] typeid:[%s] force:[%s]" url_or_path fext typeid force)
  (let* ((schema (gethash url_or_path genrnc--hash-schema-cache))
         (idx (genrnc--get-index url_or_path))
         (oldtypeid (genrnc--get-schemas-typeid-by-user idx))
         (rootp (not typeid))
         (typeid (or typeid
                     oldtypeid
                     (when (not (genrnc--user-file-p url_or_path))
                       (genrnc--read-typeid url_or_path)))))
    (yaxception:$
      (yaxception:try
        (if (and (genrnc--schema-p schema)
                 (not (eq (genrnc--schema-state schema) 'failed)))
            (progn (genrnc--info "already finished or started generate '%s'" url_or_path)
                   (when rootp
                     (message "[GENRNC] Already finished or started regist '%s'" url_or_path)))
          (puthash url_or_path (make-genrnc--schema :name url_or_path :state 'loading) genrnc--hash-schema-cache)
          (cc:dataflow-clear genrnc--dfenv url_or_path)
          (cond ((and (not force)
                      (genrnc--exist-schema url_or_path))
                 (genrnc--info "already registed '%s'" url_or_path)
                 (genrnc--finish-generate url_or_path 'succeed rootp nil typeid force))
                ((string-match "^https?://" url_or_path)
                 (genrnc--generate-from-url url_or_path fext rootp typeid force))
                ((genrnc--user-file-p url_or_path)
                 (genrnc--info "already registed user schema '%s'" url_or_path)
                 (genrnc--finish-generate url_or_path 'succeed rootp nil typeid force))
                (t
                 (genrnc--generate-from-file url_or_path fext rootp typeid force)))))
      (yaxception:catch 'error e
        (genrnc--finish-generate url_or_path 'failed rootp (yaxception:get-text e) typeid force)))))

(defun genrnc--read-typeid (url_or_path &optional initial-contents)
  (genrnc--trace "start read typeid for '%s'" url_or_path)
  (genrnc--update-defined-typeid-list)
  (genrnc--trace "got defined typeid : %s" (mapconcat 'identity genrnc--defined-typeid-list ","))
  (let* ((typeid)
         (prompt (format "[GENRNC] Input id of '%s' (used at selecting schema on nXML-mode): " url_or_path))
         (inittext (or initial-contents
                       (replace-regexp-in-string "\\.[^.]+$" ""
                                                 (replace-regexp-in-string "^.+[/\\]" "" url_or_path)))))
    (while (not typeid)
      (setq typeid (read-from-minibuffer prompt inittext))
      (setq typeid (replace-regexp-in-string "^\\s-+" "" typeid))
      (setq typeid (replace-regexp-in-string "\\s-+$" "" typeid))
      (genrnc--trace "got inputed typeid:[%s] for '%s'" typeid url_or_path)
      (cond ((or (not typeid)
                 (not (stringp typeid))
                 (string= typeid ""))
             (setq prompt "[GENRNC] Inputed empty or not string value. Re-Input id: ")
             (setq typeid nil)
             (setq inittext nil))
            ((member typeid genrnc--defined-typeid-list)
             (setq prompt (format "[GENRNC] '%s' isn't available. It's already exist. Re-Input id: " typeid))
             (setq typeid nil)
             (setq inittext nil))))
    (add-to-list 'genrnc--defined-typeid-list typeid)
    typeid))

(defun genrnc--update-defined-typeid-list ()
  (loop for f in rng-schema-locating-files
        if (and (stringp f)
                (file-exists-p f))
        do (genrnc--trace "start get defined typeid in '%s'" f)
        do (loop for rule in (rng-get-parsed-schema-locating-file f)
                 for idcons = (when (eq (car rule) 'typeId)
                                (assq 'id (cdr rule)))
                 if idcons
                 do (add-to-list 'genrnc--defined-typeid-list (cdr idcons)))))

(defun genrnc--exist-schema (url_or_path)
  (let* ((idx (genrnc--get-index url_or_path)))
    (cond ((not idx)
           (genrnc--trace "not yet indexed '%s'" url_or_path)
           nil)
          ((not (file-exists-p (genrnc--get-absolute-user-path idx ".rnc")))
           (genrnc--trace "not yet registed '%s'" url_or_path)
           nil)
          (t
           t))))

(defun genrnc--user-file-p (filepath)
  (and (stringp filepath)
       (file-exists-p filepath)
       (string= (directory-file-name (file-name-directory filepath))
                (genrnc--get-absolute-user-path))))

(defvar genrnc--regexp-http-status (rx-to-string `(and bos "HTTP/" (+ (any "0-9.")) (+ space) (group (+ (any "0-9"))))))
(defvar genrnc--regexp-http-location (rx-to-string `(and bol "Location:" (+ space) (group (+ not-newline)) eol)))
(defun genrnc--generate-from-url (url fext rootp typeid force &optional redirecturl)
  (genrnc--trace "start generate from url:[%s] fext:[%s] rootp:[%s] typeid:[%s] force:[%s] redirecturl:[%s]" url fext rootp typeid force redirecturl)
  (lexical-let* ((url url)
                 (accessurl (or redirecturl url))
                 (fext fext)
                 (rootp rootp)
                 (typeid typeid)
                 (force force))
    (message "Downloading '%s' ..." accessurl)
    (deferred:$
      (deferred:timeout genrnc-download-timeout nil (deferred:url-retrieve accessurl))
      (deferred:nextc it
        (lambda (buff)
          (if (not (buffer-live-p buff))
              (genrnc--finish-generate url 'failed rootp "timeout downloading" typeid force)
            (with-current-buffer buff
              (yaxception:$
                (yaxception:try
                  (genrnc--debug "downloaded [ %s ]\n%s" accessurl (buffer-string))
                  (goto-char (point-min))
                  (let* ((httpstat (or (when (re-search-forward genrnc--regexp-http-status nil t)
                                         (string-to-number (match-string-no-properties 1)))
                                       0)))
                    (cond ((and (>= httpstat 200)
                                (<  httpstat 300))
                           (genrnc--info "success download '%s'" accessurl)
                           (message "Download '%s' successful." accessurl)
                           (goto-char (point-min))
                           (when (re-search-forward "\n\n" nil t)
                             (genrnc--trace "delete http header of '%s'" accessurl)
                             (delete-region (point-min) (point)))
                           (genrnc--store-schema url buff fext)
                           (genrnc--generate-sentinel url fext rootp typeid force))
                          ((and (>= httpstat 300)
                                (<  httpstat 400)
                                (goto-char (point-min))
                                (re-search-forward "\n\n" nil t)
                                (re-search-backward genrnc--regexp-http-location nil t))
                           (let* ((redirecturl (match-string-no-properties 1)))
                             (genrnc--info "redirected [ %s ] to [ %s ]" accessurl redirecturl)
                             (genrnc--generate-from-url url fext rootp typeid force redirecturl)))
                          (t
                           (genrnc--finish-generate url 'failed rootp (format "failed http status : %s" httpstat) typeid force)))))
                (yaxception:catch 'error e
                  (genrnc--finish-generate url 'failed rootp (yaxception:get-text e) typeid force))
                (yaxception:finally
                  (when (buffer-live-p buff)
                    (genrnc--trace "kill download buffer of '%s'" accessurl)
                    (set-buffer-modified-p nil)
                    (kill-buffer))))))))
      (deferred:error it
        (lambda (e)
          (genrnc--finish-generate url 'failed rootp (format "%s" e) typeid force))))))

(defun genrnc--generate-from-file (path fext rootp typeid force)
  (genrnc--trace "start generate from file:[%s] fext:[%s] rootp:[%s] typeid:[%s] force:[%s]" path fext rootp typeid force)
  (let* ((path (or (file-symlink-p path)
                   path)))
    (if (or (not (file-exists-p path))
            (not (file-regular-p path))
            (not (file-readable-p path)))
        (genrnc--finish-generate path 'failed rootp "can't open" typeid force)
      (with-temp-buffer
        (insert-file-contents path)
        (genrnc--store-schema path (current-buffer) fext))
      (genrnc--generate-sentinel path fext rootp typeid force))))

(defun genrnc--finish-generate (url_or_path state rootp &optional errmsg typeid not-delete-define-p)
  (yaxception:$
    (yaxception:try
      (genrnc--info "finish generate '%s'. state:[%s] rootp:[%s] typeid:[%s] not-delete-define-p:[%s]"
                    url_or_path (symbol-name state) rootp typeid not-delete-define-p)
      (when (not (genrnc--user-file-p url_or_path))
        (let* ((idx (genrnc--get-index-with-create url_or_path))
               (schema (gethash url_or_path genrnc--hash-schema-cache)))
          (setf (genrnc--schema-state schema) state)
          (case state
            (succeed (genrnc--store-schemas-defining idx)
                     (when rootp
                       (genrnc--update-schemas-prefix-defining idx)
                       (genrnc--store-schemas-defining idx typeid)
                       (genrnc--info "registed '%s'" url_or_path)
                       (message "[GENRNC] Finished regist '%s'. Select '%s' on nXML-mode to use this schema." url_or_path typeid)))
            (failed (genrnc--error "failed generate '%s' : %s" url_or_path errmsg)
                    (when typeid
                      (genrnc--trace "remove typeid:[%s] from defined list" typeid)
                      (setq genrnc--defined-typeid-list (delete typeid genrnc--defined-typeid-list)))
                    (setf (genrnc--schema-errmsg schema) errmsg)
                    (when rootp
                      (message "[GENRNC] Failed regist '%s' : %s" url_or_path errmsg))
                    (when (not not-delete-define-p)
                      (loop for typeid in (genrnc--get-schemas-typeid-list idx)
                            do (genrnc--remove-schemas-defining typeid))))))))
    (yaxception:catch 'error e
      (genrnc--error "failed finish generate '%s' : %s" url_or_path (yaxception:get-text e))
      (when typeid
        (genrnc--trace "remove typeid:[%s] from defined list" typeid)
        (setq genrnc--defined-typeid-list (delete typeid genrnc--defined-typeid-list)))
      (let* ((schema (gethash url_or_path genrnc--hash-schema-cache)))
        (when (genrnc--schema-p schema)
          (setf (genrnc--schema-state schema) 'failed)
          (setf (genrnc--schema-errmsg schema) (yaxception:get-text e)))))
    (yaxception:finally
      (genrnc--trace "set dataflow of '%s'" url_or_path)
      (cc:dataflow-set genrnc--dfenv url_or_path (gethash url_or_path genrnc--hash-schema-cache))
      (when rootp
        (genrnc--log-open-log-if-debug)))))
  
(defun genrnc--get-index-with-create (url_or_path)
  (if (or (not (stringp url_or_path))
          (string= url_or_path ""))
      (genrnc--error "can't get index of '%s'" url_or_path)
    (or (genrnc--get-index url_or_path)
        (with-current-buffer (genrnc--get-index-file-buffer)
          (genrnc--trace "start create index entry of '%s'" url_or_path)
          (let* ((idx (format "%d" (setq genrnc--index-count (+ genrnc--index-count 1)))))
            (goto-char (point-max))
            (insert idx " " url_or_path "\n")
            (save-buffer)
            (genrnc--trace "got index [%s] from '%s'" idx url_or_path)
            idx)))))

(defun genrnc--get-index (url_or_path)
  (if (or (not (stringp url_or_path))
          (string= url_or_path ""))
      (genrnc--error "can't get index of '%s'" url_or_path)
    (with-current-buffer (genrnc--get-index-file-buffer)
      (save-excursion
        (loop initially (goto-char (point-min))
              for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
              until (eobp)
              when (and (string-match genrnc--regexp-index-line line)
                        (string= (match-string-no-properties 2 line) url_or_path))
              return (let* ((idx (match-string-no-properties 1 line)))
                       (genrnc--trace "got index [%s] from '%s'" idx url_or_path)
                       idx)
              do (forward-line 1)
              finally return nil)))))

(defun genrnc--get-index-from-typeid (typeid)
  (let* ((schemafile (loop with schemasfile = (genrnc--get-absolute-user-path genrnc--schemas-file-name)
                           for rule in (rng-get-parsed-schema-locating-file schemasfile)
                           for idcons = (when (eq (car rule) 'typeId)
                                          (assq 'id (cdr rule)))
                           for uricons = (when (eq (car rule) 'typeId)
                                           (assq 'uri (cdr rule)))
                           for currtypeid = (when idcons
                                              (cdr idcons))
                           if (and currtypeid
                                   (string= currtypeid typeid))
                           return (cdr uricons))))
    (when (string-match "^.+/\\([0-9]+\\)\\.rnc$" schemafile)
      (match-string-no-properties 1 schemafile))))

(defun genrnc--get-schema-location (idx)
  (if (or (not (stringp idx))
          (string= idx ""))
      (genrnc--error "can't get schema location of idx:[%s]" idx)
    (with-current-buffer (genrnc--get-index-file-buffer)
      (save-excursion
        (loop initially (goto-char (point-min))
              for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
              until (eobp)
              when (and (string-match genrnc--regexp-index-line line)
                        (string= (match-string-no-properties 1 line) idx))
              return (let* ((location (match-string-no-properties 2 line)))
                       (genrnc--trace "got schema location [%s] from idx:[%s]" location idx)
                       location)
              do (forward-line 1)
              finally return nil)))))

(defun genrnc--store-schemas-defining (idx &optional typeid)
  (genrnc--trace "start store schemas defining. idx:[%s] typeid:[%s]" idx typeid)
  (let* ((nscons (genrnc--get-schema-namespace idx))
         (prefix (or (when nscons
                       (car nscons))
                     ""))
         (ns (when nscons
               (cdr nscons)))
         (localnm (or (when (stringp typeid)
                        (replace-regexp-in-string " " "_" typeid))
                      (concat "genrnc-" idx)))
         (typeid (or typeid
                     (concat "[genrnc-" idx "]"))))
    (genrnc--remove-schemas-defining typeid)
    (with-current-buffer (genrnc--get-schemas-file-buffer)
      (goto-char (point-max))
      (when (re-search-backward "</locatingRules>" nil t)
        (insert "<uri pattern=\"\" typeId=\"" typeid "\"/>\n")
        (when ns
          (insert "<namespace ns=\"" ns "\" typeId=\"" typeid "\"/>\n"))
        (insert "<documentElement prefix=\"" prefix "\" localName=\"" localnm "\" typeId=\"" typeid "\"/>\n")
        (insert "<typeId id=\"" typeid "\" uri=\"" idx ".rnc\"/>\n")
        (indent-region (point-min) (point-max))
        (save-buffer)
        (genrnc--trace "stored schemas defining. idx:[%s] typeid:[%s]" idx typeid)))))

(defvar genrnc--regexp-schema-namespace (rx-to-string `(and bol (* space) (? "default" (+ space))
                                                            "namespace" (group (+ (not (any "=")))) "=" (+ space)
                                                            "\"" (group (+ (not (any "\"")))) "\"")))
(defun genrnc--get-namespace-alist-in-schema-file (idx)
  (genrnc--trace "start get namespace alist of idx:[%s]" idx)
  (let* ((schemafile (genrnc--get-absolute-user-path idx ".rnc")))
    (with-temp-buffer
      (insert-file-contents schemafile)
      (loop with xsdre = (rx-to-string `(and bos "http://www.w3.org/" (+ (not (any "\"" "'"))) "/XMLSchema" eos))
            initially (goto-char (point-min))
            while (re-search-forward genrnc--regexp-schema-namespace nil t)
            for prefix = (match-string-no-properties 1)
            for ns = (match-string-no-properties 2)
            for prefix = (replace-regexp-in-string "^\\s-+" "" prefix)
            for prefix = (replace-regexp-in-string "\\s-+$" "" prefix)
            if (and (not (string-match "^http://relaxng\\.org/ns/compatibility/annotations" ns))
                    (not (string-match "^http://relaxng\\.org/ns/structure" ns))
                    (not (string-match xsdre ns)))
            collect (progn (genrnc--trace "got namespace:[%s] prefix:[%s] of idx:[%s]" ns prefix idx)
                           (cons prefix ns))))))

(defun genrnc--get-schema-namespace (idx)
  (genrnc--trace "start get schema namespace of idx:[%s]" idx)
  (let* ((nsalist (genrnc--get-namespace-alist-in-schema-file idx))
         (ret (or (when nsalist
                    (assoc "" nsalist))
                  (when nsalist
                    (pop nsalist)))))
    (genrnc--trace "got schema namespace of [%s] : %s" idx ret)
    ret))

(defun genrnc--update-schemas-prefix-defining (idx)
  (genrnc--trace "start update schemas prefix defining of '%s'" idx)
  (with-current-buffer (genrnc--get-schemas-file-buffer)
    (loop for nscons in (genrnc--get-namespace-alist-in-schema-file idx)
          for prefix = (car nscons)
          for ns = (cdr nscons)
          for re = (rx-to-string `(and "<namespace" (+ space) "ns=\"" ,ns "\"" (+ space) "typeId=\"" (group (+ (not (any "\"")))) "\""))
          do (goto-char (point-min))
          if (and (stringp prefix)
                  (not (string= prefix "")))
          do (while (re-search-forward re nil t)
               (let* ((typeid (match-string-no-properties 1))
                      (re (rx-to-string `(and (group "<documentElement" (+ space) "prefix=\"")
                                              (group (* (not (any "\""))))
                                              (group "\"" (+ (not (any ">"))) bow "typeId=\"" ,typeid "\"")))))
                 (goto-char (point-min))
                 (while (re-search-forward re nil t)
                   (let* ((beforetext (match-string-no-properties 1))
                          (currprefix (match-string-no-properties 2))
                          (aftertext (match-string-no-properties 3)))
                     (when (string= currprefix "")
                       (genrnc--info "update schemas prefix defining of '%s' from [%s] to [%s]" typeid currprefix prefix)
                       (replace-match (concat beforetext prefix aftertext)))))))
          finally do (save-buffer))))

(defun genrnc--remove-schema (idx &optional remove-definition-p fext)
  (if (or (not (stringp idx))
          (string= idx ""))
      (genrnc--warn "can't remove schema of idx:[%s]" idx)
    (genrnc--info "start remove schema. idx:[%s] remove-definition-p:[%s] fext:[%s]" idx remove-definition-p fext)
    (loop for fext in (cond (remove-definition-p '("xsd" "rng" "dtd" "rnc"))
                            (fext (list fext))
                            (t           '("xsd" "rng" "dtd")))
          for path = (genrnc--get-absolute-user-path idx "." fext)
          if (file-exists-p path)
          do (progn (genrnc--trace "delete '%s'" path)
                    (delete-file path)))
    (when remove-definition-p
      (loop for typeid in (genrnc--get-schemas-typeid-list idx)
            do (genrnc--remove-schemas-defining typeid)))))

(defun genrnc--remove-schema-all (&optional remove-definition-p)
  (genrnc--info "start remove schema all. remove-definition-p:[%s]" remove-definition-p)
  (when (file-directory-p (genrnc--get-absolute-user-path))
    (let* ((idxlist))
      (loop for f in (directory-files (genrnc--get-absolute-user-path))
            if (string-match "^\\([0-9]+\\)\\.[a-z]+$" f)
            do (add-to-list 'idxlist (match-string-no-properties 1 f)))
      (loop for idx in idxlist
            do (genrnc--remove-schema idx remove-definition-p)))))

(defun genrnc--get-schemas-typeid-list (idx)
  (when (and (stringp idx)
             (not (string= idx "")))
    (with-current-buffer (genrnc--get-schemas-file-buffer)
      (save-excursion
        (loop with re = (rx-to-string `(and bol (* space) "<typeId"
                                            (+ space) "id=\"" (group (+ (not (any "\"")))) "\""
                                            (+ space) "uri=\"" ,idx ".rnc\""))
              initially (goto-char (point-min))
              while (re-search-forward re nil t)
              collect (match-string-no-properties 1))))))

(defvar genrnc--regexp-schemas-typeid-excluded (rx-to-string `(and bos "[genrnc-" (+ numeric) "]" eos)))
(defun genrnc--get-schemas-typeid-by-user (idx)
  (loop for e in (genrnc--get-schemas-typeid-list idx)
        if (not (string-match genrnc--regexp-schemas-typeid-excluded e))
        return e))

(defun genrnc--get-schemas-typeid-list-by-user ()
  (loop with schemasfile = (genrnc--get-absolute-user-path genrnc--schemas-file-name)
        for rule in (rng-get-parsed-schema-locating-file schemasfile)
        for idcons = (when (eq (car rule) 'typeId)
                       (assq 'id (cdr rule)))
        for id = (when idcons
                   (cdr idcons))
        if (and id
                (not (string-match genrnc--regexp-schemas-typeid-excluded id)))
        collect id))

(defun genrnc--remove-schemas-defining (typeid)
  (if (not (stringp typeid))
      (genrnc--error "can't remove schemas defining of '%s'" typeid)
    (genrnc--info "start remove schemas defining of [%s]" typeid)
    (with-current-buffer (genrnc--get-schemas-file-buffer)
      (loop with re1 = (rx-to-string `(and "typeId=\"" ,typeid "\""))
            with re2 = (rx-to-string `(and "<typeId" (+ space) "id=\"" ,typeid "\""))
            initially (goto-char (point-min))
            for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
            until (eobp)
            if (or (string-match re1 line)
                   (string-match re2 line))
            do (progn (delete-region (point-at-bol) (point-at-eol))
                      (delete-char 1))
            else
            do (forward-line 1)
            finally do (save-buffer)))))

(defun genrnc--store-schema (url_or_path buff fext)
  (genrnc--trace "start store schema '%s'. fext:[%s]" url_or_path fext)
  (let* ((idx (genrnc--get-index-with-create url_or_path)))
    (with-current-buffer buff
      (write-file (genrnc--get-absolute-user-path idx "." fext) nil)
      (set-buffer-modified-p nil)
      (kill-buffer)
      (genrnc--info "stored schema '%s'" url_or_path))))

(yaxception:deferror 'genrnc--include-error nil "can't include '%s'" 'include-value)
(defun genrnc--generate-sentinel (url_or_path fext rootp typeid force)
  (yaxception:$
    (yaxception:try
      (genrnc--trace "start generate sentinel to '%s'. fext:[%s] rootp:[%s] typeid:[%s] force:[%s]" url_or_path fext rootp typeid force)
      (lexical-let* ((url_or_path url_or_path)
                     (fext fext)
                     (rootp rootp)
                     (typeid typeid)
                     (force force)
                     (idx (genrnc--get-index-with-create url_or_path))
                     (schemafile (genrnc--get-absolute-user-path idx "." fext))
                     (openp (loop for b in (buffer-list)
                                  for filenm = (buffer-file-name b)
                                  if (and filenm
                                          (string= filenm schemafile))
                                  return t
                                  finally return nil))
                     (schemabuff (find-file-noselect schemafile))
                     (includings (genrnc--get-and-replace-including-contents schemabuff url_or_path fext))
                     (abbr-includings (genrnc--get-and-replace-abbreviated-including-contents schemabuff url_or_path fext)))
        (genrnc--complement-namespace-info schemabuff url_or_path fext)
        (when (and (not openp)
                   (buffer-live-p schemabuff))
          (genrnc--trace "kill buffer of not opened '%s'" schemafile)
          (kill-buffer schemabuff))
        (cond ((and (not includings)
                    (not abbr-includings))
               (genrnc--convert-schema url_or_path idx fext rootp typeid force))
              (t
               (when includings
                 (genrnc--trace "start generate includings : %s" (mapconcat 'identity includings ","))
                 (loop for e in includings
                       do (genrnc--generate e fext typeid force)))
               (when abbr-includings
                 (genrnc--trace "start generate abbreviate includings : %s" (mapconcat 'identity abbr-includings ","))
                 (loop for e in abbr-includings
                       do (genrnc--generate e fext)))
               (deferred:$
                 (deferred:parallel-list
                   (loop for e in (append includings abbr-includings)
                         if (stringp e)
                         collect (cc:dataflow-get genrnc--dfenv e)))
                 (deferred:nextc it
                   (lambda (schemas)
                     (genrnc--trace "generated all includings : %s" (mapconcat 'identity (append includings abbr-includings) ","))
                     (loop for s in schemas
                           do (cond ((not (genrnc--schema-p s))
                                     (yaxception:throw 'genrnc--include-error :include-value "=UNKNOWN="))
                                    ((eq (genrnc--schema-state s) 'failed)
                                     (yaxception:throw 'genrnc--include-error :include-value (genrnc--schema-name s)))))
                     (genrnc--convert-schema url_or_path idx fext rootp typeid force)))
                 (deferred:error it
                   (lambda (e)
                     (genrnc--finish-generate url_or_path 'failed rootp (format "%s" e) typeid force))))))))
    (yaxception:catch 'error e
      (genrnc--finish-generate url_or_path 'failed rootp (yaxception:get-text e) typeid force))))

(defvar genrnc--regexp-dtd-entitynm "[a-zA-Z][a-zA-Z0-9_.-]*")
(defvar genrnc--regexp-dtd-include (rx-to-string `(and (group "<!" (* space) "ENTITY" (+ space)
                                                              "%" (+ space) (regexp ,genrnc--regexp-dtd-entitynm) (+ space)
                                                              "PUBLIC" (+ space) "\"" (+ (not (any "\""))) "\"" (+ space) "\"")
                                                       (group (+ (not (any "\""))))
                                                       (group "\"" (* space) ">"))))
(defvar genrnc--regexp-rnc-include (rx-to-string `(and bol (group (* space) "include" (+ (not (any "\"" "'"))))
                                                       (group (+ (not (any "\"" "'"))))
                                                       (group (or "\"" "'")))))
(defun genrnc--get-and-replace-including-contents (buff url_or_path fext)
  (genrnc--trace "start get and replace includings of '%s'. fext:[%s]" url_or_path fext)
  (with-current-buffer buff
    (let* ((re (cond ((string= fext "xsd") (genrnc--get-regexp-xsd-import buff))
                     ((string= fext "rng") (genrnc--get-regexp-rng-include buff))
                     ((string= fext "dtd") genrnc--regexp-dtd-include)
                     ((string= fext "rnc") genrnc--regexp-rnc-include))))
      (when re
        (loop initially (progn (goto-char (point-min))
                               (when (not (string-match "^https?://" url_or_path))
                                 (cd (file-name-directory url_or_path))))
              with parenturl = (replace-regexp-in-string "/[^/]+$" "/" url_or_path)
              while (re-search-forward re nil t)
              for begintext = (match-string-no-properties 1)
              for including = (match-string-no-properties 2)
              for aftertext = (match-string-no-properties 3)
              for mdata = (match-data)
              for including = (progn
                                (genrnc--trace "found including '%s' in '%s'" including url_or_path)
                                (cond ((string-match "^https?://" url_or_path)
                                       ;; If registing schema is URL, format including contents to URL.
                                       (cond ((string-match "^https?://" including)
                                              including)
                                             (t
                                              (concat parenturl including))))
                                      ((not (string-match "^https?://" including))
                                       ;; If registing schema is PATH, and including contents isn't URL, format it to absolute path.
                                       (expand-file-name including))
                                      (t
                                       including)))
              for idx = (genrnc--get-index-with-create including)
              do (set-match-data mdata)
              do (replace-match (concat begintext idx "." fext aftertext))
              do (genrnc--trace "got including '%s' and replaced to [%s]" including idx)
              collect including
              finally do (save-buffer))))))

(defvar genrnc--regexp-xsd-xmlns (rx-to-string `(and "<" (+ (not (any ">")))
                                                     bow "xmlns" (group (* (not (any "=")))) "=" (or "\"" "'")
                                                     "http://www.w3.org/" (+ (not (any "\"" "'"))) "/XMLSchema\"")))
(defun genrnc--get-regexp-xsd-import (buff)
  (genrnc--trace "start get regexp for xsd import")
  (with-current-buffer buff
    (goto-char (point-min))
    (if (not (re-search-forward genrnc--regexp-xsd-xmlns nil t))
        (genrnc--error "not found define xmlns of XMLSchema in following text ...\n%s" (buffer-string))
      (let* ((prefix (match-string-no-properties 1))
             (prefix (cond ((and (stringp prefix)
                                 (not (string= prefix "")))
                            (concat (replace-regexp-in-string "^:" "" prefix) ":"))
                           (t
                            ""))))
        (genrnc--trace "found xmlns prefix for XMLSchema is [%s]" prefix)
        (rx-to-string `(and (group "<" ,prefix (or "import" "include") (+ (not (any ">"))) bow "schemaLocation=" (or "\"" "'"))
                            (group (+ (not (any "\"" "'"))))
                            (group (or "\"" "'"))))))))

(defvar genrnc--regexp-rng-xmlns (rx-to-string `(and "<" (+ (not (any ">")))
                                                     bow "xmlns" (group (* (not (any "=")))) "=" (or "\"" "'")
                                                     "http://relaxng.org/ns/structure/")))
(defun genrnc--get-regexp-rng-include (buff)
  (genrnc--trace "start get regexp for rng include")
  (with-current-buffer buff
    (goto-char (point-min))
    (if (not (re-search-forward genrnc--regexp-rng-xmlns nil t))
        (genrnc--error "not found define xmlns of RelaxNG in following text ...\n%s" (buffer-string))
      (let* ((prefix (match-string-no-properties 1))
             (prefix (cond ((and (stringp prefix)
                                 (not (string= prefix "")))
                            (concat (replace-regexp-in-string "^:" "" prefix) ":"))
                           (t
                            ""))))
        (genrnc--trace "found xmlns prefix for RelaxNG is [%s]" prefix)
        (rx-to-string `(and (group "<" ,prefix "include" (+ (not (any ">"))) bow "href=" (or "\"" "'"))
                            (group (+ (not (any "\"" "'"))))
                            (group (or "\"" "'"))))))))

(defun genrnc--get-regexp-xsd-nolocated-import (buff)
  (with-current-buffer buff
    (goto-char (point-min))
    (when (re-search-forward genrnc--regexp-xsd-xmlns nil t)
      (genrnc--trace "start get regexp for xsd import not schemaLocation")
      (let* ((prefix (match-string-no-properties 1))
             (prefix (cond ((and (stringp prefix)
                                 (not (string= prefix "")))
                            (concat (replace-regexp-in-string "^:" "" prefix) ":"))
                           (t
                            ""))))
        (rx-to-string `(and (group "<" ,prefix "import" (+ (not (any ">"))) bow "namespace=" (or "\"" "'"))
                            (group (+ (not (any "\"" "'"))))
                            (group (or "\"" "'"))
                            (group (* (not (any ">"))) ">")))))))

(defun genrnc--get-and-replace-abbreviated-including-contents (buff url_or_path fext)
  (genrnc--trace "start get and replace abbreviated includings of '%s'. fext:[%s]" url_or_path fext)
  (let* ((re (genrnc--get-regexp-xsd-nolocated-import buff)))
    (when (and re (string= fext "xsd"))
      (with-current-buffer buff
        (loop initially (goto-char (point-min))
              while (re-search-forward re nil t)
              for begintext = (match-string-no-properties 1)
              for ns = (match-string-no-properties 2)
              for aftertext1 = (match-string-no-properties 3)
              for aftertext2 = (match-string-no-properties 4)
              for mdata = (match-data)
              do (genrnc--trace "found abbreviated including for '%s' in '%s'" ns url_or_path)
              if (and (not (string-match "schemaLocation=" begintext))
                      (not (string-match "schemaLocation=" aftertext2))
                      (save-excursion
                        (let* ((re (rx-to-string `(and "<" (+ (not (any ">"))) bow "xmlns" (? ":" (+ (any "a-z"))) "="
                                                       (or "\"" "'") ,ns (or "\"" "'")))))
                          (goto-char (point-min))
                          (re-search-forward  re nil t))))
              append (let* ((prompt (format "[GENRNC] Including not located ns:'%s'. Input PATH or URL locating this: " ns))
                            (nslocates (or (gethash ns genrnc--hash-namespace-files)
                                           (list (read-string prompt ns))))
                            (userim (not (gethash ns genrnc--hash-namespace-files))))
                       (genrnc--trace "got locate files for '%s' : %s" ns (mapconcat 'identity nslocates ","))
                       (set-match-data mdata)
                       (replace-match "")
                       (loop for loc in nslocates
                             for idx = (when userim
                                         (when (not (string-match "^https?://" loc))
                                           (setq loc (expand-file-name loc))
                                           (when (not (file-exists-p loc))
                                             (setq loc nil)))
                                         (genrnc--get-index-with-create loc))
                             do (cond ((and userim
                                            (stringp idx)
                                            (stringp loc))
                                       (insert (format "%s%s%s schemaLocation=\"%s\"%s"
                                                       begintext ns aftertext1 (concat idx "." fext) aftertext2)))
                                      ((and (not userim)
                                            (file-exists-p loc))
                                       (insert (format "%s%s%s schemaLocation=\"%s\"%s"
                                                       begintext ns aftertext1
                                                       (replace-regexp-in-string "\\\\" "\\\\"
                                                                                 (replace-regexp-in-string "rnc$" fext loc))
                                                       aftertext2)))
                                      (t
                                       (genrnc--error "can't insert import element. value:[%s] user-input:[%s] idx:[%s]" loc userim idx)))
                             if (stringp loc)
                             collect loc))
              finally do (save-buffer))))))

(defun genrnc--complement-namespace-info (buff url_or_path fext)
  (genrnc--trace "start complement namespace info of '%s'. fext:[%s]" url_or_path fext)
  (when (string= fext "xsd")
    (with-current-buffer buff
      (let* ((schemare (rx-to-string `(and "<" (? (+ (any "a-z")) ":") "schema" (+ space) (+ (not (any ">"))) ">")))
             (defnsre1 (rx-to-string `(and bow "targetNamespace=" (or "\"" "'") (group (+ (not (any "\"" "'")))) (or "\"" "'"))))
             (defnsre2 (rx-to-string `(and bow "xmlns=" (or "\"" "'") (group (+ (not (any "\"" "'")))) (or "\"" "'"))))
             (xmlnsre (rx-to-string `(and bow "xmlns:" (group (+ (any "a-z"))) "="
                                          (or "\"" "'") (group (+ (not (any "\"" "'")))) (or "\"" "'"))))
             (xsdre (rx-to-string `(and "http://www.w3.org/" (+ (not (any "\"" "'"))) "/XMLSchema")))
             (xsdprefix (or (and (goto-char (point-min))
                                 (re-search-forward genrnc--regexp-xsd-xmlns nil t)
                                 (not (string= (match-string-no-properties 1) ""))
                                 (concat (replace-regexp-in-string "^:" "" (match-string-no-properties 1)) ":"))
                            ""))
             (nsalist))
        (goto-char (point-min))
        (if (not (re-search-forward schemare nil t))
            (genrnc--error "not found schema element of '%s'" url_or_path)
          (loop for re in (list defnsre1 defnsre2)
                for found = (and (re-search-backward re nil t)
                                 (let* ((ns (match-string-no-properties 1)))
                                   (when (not (string-match xsdre ns))
                                     (push (cons "" ns) nsalist))))
                if found return nil
                else do (progn (goto-char (point-min))
                               (re-search-forward schemare nil t)))
          (goto-char (point-min))
          (re-search-forward schemare nil t)
          (loop while (re-search-backward xmlnsre nil t)
                do (let* ((prefix (match-string-no-properties 1))
                          (ns (match-string-no-properties 2)))
                     (when (not (string-match xsdre ns))
                       (push (cons prefix ns) nsalist))))
          (genrnc--trace "found namespace of '%s' : %s" url_or_path nsalist)
          (goto-char (point-min))
          (re-search-forward schemare nil t)
          (insert "\n\n")
          (loop for nscons in nsalist
                for prefix = (car nscons)
                for ns = (cdr nscons)
                for urlformat = (string-match "^https?://" ns)
                for ns = (replace-regexp-in-string "^https?://" "" ns)
                for ns = (replace-regexp-in-string "[^a-zA-Z0-9-_./:]" "" ns)
                for ns = (replace-regexp-in-string ":" "." ns)
                for e = (split-string ns "/")
                for host = (pop e)
                for hoste = (cond (urlformat (reverse (split-string host "\\.")))
                                  (t         (split-string host "\\.")))
                for path = (mapconcat 'identity e ".")
                for nsident = (concat (mapconcat 'identity hoste ".") "." path)
                for nsident = (replace-regexp-in-string "[.]+" "." nsident)
                for nsident = (replace-regexp-in-string "/+" "/" nsident)
                for nsident = (replace-regexp-in-string "^[^a-zA-Z]+" "" nsident)
                for nsident = (replace-regexp-in-string "[-_.]+$" "" nsident)
                do (genrnc--trace "insert genrnc element. prefix:[%s] name:[%s] value:[%s]" xsdprefix prefix nsident)
                do (insert (format "<%sgenrnc name=\"%s\">%s</%sgenrnc>\n" xsdprefix prefix nsident xsdprefix)))
          (insert "\n")
          (save-buffer))))))

(defun genrnc--convert-schema (url_or_path idx fext rootp typeid force)
  (genrnc--trace "start convert schema '%s'. idx:[%s] fext:[%s] rootp:[%s] typeid:[%s] force:[%s]" url_or_path idx fext rootp typeid force)
  (when (string= fext "xsd")
    (genrnc--do-xsd2rng idx force)
    (setq fext "rng"))
  (when (not (string= fext "rnc"))
    (genrnc--do-trang idx fext force))
  (genrnc--finish-generate url_or_path 'succeed rootp nil typeid))

(yaxception:deferror 'genrnc--convert-error nil "failed convert by %s : %s" 'cmd 'stderr)
(defun genrnc--do-trang (idx fext force)
  (genrnc--trace "start trang idx:[%s] fext:[%s]" idx fext)
  (cd (genrnc--get-absolute-user-path))
  (when force
    (genrnc--remove-schema idx nil "rnc"))
  (let* ((outfile (concat idx ".rnc"))
         (cmdstr (format "java -jar %s %s %s" genrnc--trang-jar-name (concat idx "." fext) outfile))
         (ret (shell-command-to-string cmdstr)))
    (genrnc--trace "done trang : %s\n%s" cmdstr ret)
    (when (or (not (file-exists-p outfile))
              (= (nth 7 (file-attributes outfile)) 0))
      (yaxception:throw 'genrnc--convert-error :cmd "trang" :stderr ret))))

(defun genrnc--do-xsd2rng (idx force)
  (genrnc--trace "start xsd2rng idx:[%s]" idx)
  (cd (genrnc--get-absolute-user-path))
  (when force
    (genrnc--remove-schema idx nil "rng"))
  (let* ((outfile (concat idx ".rng"))
         (cmdstr (format "java -jar %s %s %s > %s" genrnc--xsd2rng-jar-name (concat idx ".xsd") genrnc--xsd2rng-xsl-name outfile))
         (ret (shell-command-to-string cmdstr)))
    (genrnc--trace "done xsd2rng : %s\n%s" cmdstr ret)
    (when (or (not (file-exists-p outfile))
              (= (nth 7 (file-attributes outfile)) 0))
      (yaxception:throw 'genrnc--convert-error :cmd "xsd2rng" :stderr ret))))


(defadvice rng-possible-type-ids-using (after exclude-genrnc-type-id activate)
  (let* ((typeids ad-return-value))
    (setq ad-return-value (loop for typeid in typeids
                                if (not (string-match genrnc--regexp-schemas-typeid-excluded typeid))
                                collect typeid))))


(add-hook 'nxml-mode-hook
          (lambda ()
            (add-to-list 'rng-schema-locating-files (genrnc--get-absolute-user-path genrnc--schemas-file-name)))
          t)


(provide 'genrnc)
