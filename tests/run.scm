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
    (test-assert (BGR2GRAY img))
    (test-assert (canny (BGR2GRAY img) 100 100 3))
    (test '(223 310) (get-size img))))

(test-group "storage"
  (test-assert (make-mem-storage 0)))

(test-group "contours"
  (test-assert CV_RETR_LIST)
  (test-assert CV_CHAIN_APPROX_SIMPLE)
  (let* ((img (load-image "cremate.jpg"))
         (gray (BGR2GRAY img))
         (mode CV_RETR_LIST)
         (method CV_CHAIN_APPROX_SIMPLE))
    (test-assert (find-contours gray mode method))))

(test-group "matrix"
  (test-assert (make-mat 10 10 CV_8UC1))
  (let ((mat (make-mat-from-buffer "foo")))
    (test #\f (integer->char (u8mat-ref mat 0 0)))
    (test #\o (integer->char (u8mat-ref mat 0 1)))
    (test #\o (integer->char (u8mat-ref mat 0 2)))
    (test '(3 1) (get-size mat))))

(test-end)
(test-exit)
