(require 'genrnc)
(require 'el-expectations)

(expectations
  (desc "get-absolute-user-path default")
  (expect "/home/.emacs.d/schema"
    (ad-with-auto-activation-disabled
     (flet ((expand-file-name (name &optional default-directory)
                              name))
       (let* ((genrnc-user-schemas-directory "/home/.emacs.d/schema"))
         (genrnc--get-absolute-user-path))))))

(expectations
  (desc "get-absolute-user-path single arg")
  (expect "/home/.emacs.d/schema/index.txt"
    (ad-with-auto-activation-disabled
     (flet ((expand-file-name (name &optional default-directory)
                              name))
       (let* ((genrnc-user-schemas-directory "/home/.emacs.d/schema"))
         (genrnc--get-absolute-user-path "index.txt"))))))

(expectations
  (desc "get-absolute-user-path multi arg")
  (expect "/home/.emacs.d/schema/1.rnc"
    (ad-with-auto-activation-disabled
     (flet ((expand-file-name (name &optional default-directory)
                              name))
       (let* ((genrnc-user-schemas-directory "/home/.emacs.d/schema"))
         (genrnc--get-absolute-user-path "1" "." "rnc"))))))

