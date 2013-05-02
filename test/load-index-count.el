(require 'genrnc)
(require 'tenv)
(require 'el-expectations)

(expectations
  (desc "load-index-count not exist")
  (expect 0
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir)
           (genrnc--index-count 0))
      (tenv-update-file ifile t "\n")
      (genrnc--load-index-count)
      genrnc--index-count)))

(expectations
  (desc "load-index-count single line")
  (expect 1
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir)
           (genrnc--index-count 0))
      (tenv-update-file ifile nil
                        "1 http://www.w3.org/Math/XMLSchema/mathml3/mathml3.xsd\n")
      (genrnc--load-index-count)
      genrnc--index-count)))

(expectations
  (desc "load-index-count multi lines with sort")
  (expect 3
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir)
           (genrnc--index-count 0))
      (tenv-update-file ifile nil
                        "2 http://www.w3.org/Math/XMLSchema/mathml3/mathml3-content.xsd\n"
                        "3 http://www.w3.org/Math/XMLSchema/mathml3/mathml3-presentation.xsd\n")
      (genrnc--load-index-count)
      genrnc--index-count)))

(expectations
  (desc "load-index-count multi lines with not sort")
  (expect 21
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir)
           (genrnc--index-count 0))
      (tenv-update-file ifile nil
                        "21 http://search.yahooapis.jp/WebSearchService/V2/WebSearchResponse.xsd\n"
                        "5 http://www.w3.org/Math/XMLSchema/mathml3/mathml3-strict-content.xsd\n")
      (genrnc--load-index-count)
      genrnc--index-count)))

(expectations
  (desc "load-index-count already loaded")
  (expect 21
    (let* ((tdir (tenv-get-tmp-directory "genrnc" t t))
           (ifile (tenv-get-tmp-file "genrnc" genrnc--index-file-name))
           (genrnc-user-schemas-directory tdir)
           (genrnc--index-count 21))
      (tenv-update-file ifile nil
                        "22 http://search.yahooapis.jp/WebSearchServicePro/V1/WebSearchResponse.xsd\n")
      (genrnc--load-index-count)
      genrnc--index-count)))

