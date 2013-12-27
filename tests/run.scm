(use opencv-highgui test srfi-1 posix)

(test-begin "opencv-highgui")

(test-group "windows"
  (test-assert (make-window "foo"))
  (test-assert (load-image "cremate.jpg")))
