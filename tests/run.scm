(use opencv opencv-highgui test srfi-1 posix)

(test-begin "opencv-highgui")

(test-group "windows"
  (test-assert (make-window "foo"))
  (test-assert (load-image "cremate.jpg"))

  (let ((window (make-window "aaron")))
    (test-assert (show-image window (load-image "cremate.jpg"))))

  (let ((window (make-window "tenderlove")))
    (test-assert (show-image "tenderlove" (load-image "cremate.jpg")))))

(test-group "matrix"
  (test-assert (make-mat 10 10 CV_8UC1)))

(test-end)
(test-exit)
