(defsystem lqdapi-wrapper
  :version "0.1"
  :license "MIT"
  :depends-on (:drakma
               :ironclad
               :cl-json
	       :cl-json-web-tokens
	       :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "lqdapi-wrapper")))))
