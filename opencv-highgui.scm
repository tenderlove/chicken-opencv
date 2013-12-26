
(module opencv-highgui
  (make-window
   destroy-window-named
   destroy-all-windows)

(import scheme chicken foreign)
(use lolevel)

(foreign-declare "#include <opencv/highgui.h>")

(define CV_WINDOW_AUTOSIZE (foreign-value "CV_WINDOW_AUTOSIZE" int))

(define (make-window name)
  (cvNamedWindow name CV_WINDOW_AUTOSIZE))

(define (destroy-window-named name) (cvDestroyWindow name))
(define (destroy-all-windows) (cvDestroyAllWindows))

(define cvNamedWindow (foreign-lambda int
                                      "cvNamedWindow"
                                      nonnull-c-string
                                      int))

(define cvDestroyWindow (foreign-lambda void
                                      "cvDestroyWindow"
                                      nonnull-c-string))

(define cvDestroyAllWindows (foreign-lambda void
                                      "cvDestroyAllWindows"))
)
