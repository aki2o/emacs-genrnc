(require 'genrnc)
(require 'tenv)
(require 'auto-complete-nxml)
(require 'el-expectations)

(expectations
  (desc "genrnc system dtd url")
  (expect '("html")
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (genrnc-user-schemas-directory tdir)
           (tfile (tenv-get-tmp-file "genrnc_system" "test.html" t t))
           (cnt 0))
      (genrnc--init-schema-directory t)
      (stub genrnc--read-typeid => "STRICT")
      (stub y-or-n-p => t)
      (genrnc-clear-cache)
      (genrnc--log-enable-logging)
      (genrnc--log-clear-log)
      (genrnc-regist-url "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
      (while (and (< cnt 60)
                  (not (with-current-buffer " *log4e-genrnc*"
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward
                            "finish generate 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'. state:" nil t)))))
        (incf cnt)
        (sleep-for 1))
      (with-current-buffer (find-file-noselect tfile)
        (rng-set-document-type-and-validate "STRICT")
        (erase-buffer)
        (goto-char (point-min))
        (insert "<")
        (auto-complete-nxml-get-candidates)))))

