(use opencv test srfi-1 posix opencv)

(test-begin "opencv-highgui")

(test-group "windows"
  (test-assert (make-window "foo"))
  (test-assert (load-image "cremate.jpg"))

  (let ((window (make-window "aaron")))
    (test-assert (show-image window (load-image "cremate.jpg"))))

  (let ((window (make-window "tenderlove")))
    (test-assert (show-image "tenderlove" (load-image "cremate.jpg"))))
  (let ((mat (make-mat-from-buffer (read-all "cremate.jpg"))))
    (test-assert (decode-image mat)))
  (let ((bytes (read-all "cremate.jpg")))
    (test-assert (decode-image bytes)))
  (let ((img (decode-image (read-all "cremate.jpg"))))
    (test '(223 310) (get-size img))))

(test-group "matrix"
  (test-assert (make-mat 10 10 CV_8UC1))
  (let ((mat (make-mat-from-buffer "foo")))
    (test #\f (integer->char (u8mat-ref mat 0 0)))
    (test #\o (integer->char (u8mat-ref mat 0 1)))
    (test #\o (integer->char (u8mat-ref mat 0 2)))
    (test '(3 1) (get-size mat))))

(test-end)
(test-exit)
