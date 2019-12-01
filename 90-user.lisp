(do
 (defun foo (x)
   (do (print "foo") (print x) x))
 (foo (foo "bar"))
 (foo 1)
 (foo 2)
 (foo 3))
