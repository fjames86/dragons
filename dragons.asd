;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :dragons
  :name "dragons"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "A DNS client."
  :license "MIT"
  :version "1.1.0"
  :serial t
  :components
  ((:file "package")
   (:file "structures")
   (:file "database")
   (:file "client"))
  :depends-on (:usocket :flexi-streams :nibbles :babel :pounds))

