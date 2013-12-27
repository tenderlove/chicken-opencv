(module opencv-highgui
  (make-window
   load-image
   destroy-window-named
   destroy-all-windows)

(import scheme chicken foreign)
(use lolevel)

(define-record-type IplImage
  (wrap-IplImage pointer)
  IplImage?
  (pointer unwrap-IplImage))

(foreign-declare "#include <opencv/highgui.h>")

(define CV_WINDOW_AUTOSIZE (foreign-value "CV_WINDOW_AUTOSIZE" int))
(define CV_WINDOW_NORMAL (foreign-value "CV_WINDOW_NORMAL" int))
(define CV_WINDOW_OPENGL (foreign-value "CV_WINDOW_OPENGL" int))
(define CV_LOAD_IMAGE_COLOR (foreign-value "CV_LOAD_IMAGE_COLOR" int))

(define-foreign-type IplImage "IplImage")
(define-foreign-type IplImage* (c-pointer "IplImage"))

(define (load-image file)
  (let ((ptr (cvLoadImage file CV_LOAD_IMAGE_COLOR)))
    (set-finalizer! ptr release-image)
    (wrap-IplImage ptr)))

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

(define cvLoadImage (foreign-lambda IplImage*
                                    "cvLoadImage"
                                    nonnull-c-string
                                    int))

(define (release-image ptr)
  (let-location ((i IplImage* ptr))
                (cvReleaseImage (location i))))

(define cvReleaseImage (foreign-lambda void
                                    "cvReleaseImage"
                                    (c-pointer IplImage*)))
)
