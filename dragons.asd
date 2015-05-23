;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :dragons
  :name "dragons"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An DNS client."
  :license "MIT"
  :components
  ((:file "dragons"))
  :depends-on (:usocket :flexi-streams :nibbles :babel))

