(module opencv-highgui
  (make-window
   load-image
   decode-image
   wait-key
   show-image
   destroy-window-named
   destroy-all-windows)

(import scheme chicken foreign opencv)
(use lolevel opencv)

(define-record-type IplImage
  (wrap-IplImage pointer)
  IplImage?
  (pointer unwrap-IplImage))

(define-record-type namedWindow
  (wrap-named-window name)
  window?
  (name unwrap-named-window))

(foreign-declare "#include <opencv/highgui.h>")

(define CV_WINDOW_AUTOSIZE (foreign-value "CV_WINDOW_AUTOSIZE" int))
(define CV_WINDOW_NORMAL (foreign-value "CV_WINDOW_NORMAL" int))
(define CV_WINDOW_OPENGL (foreign-value "CV_WINDOW_OPENGL" int))
(define CV_LOAD_IMAGE_COLOR (foreign-value "CV_LOAD_IMAGE_COLOR" int))

(define-foreign-type IplImage "IplImage")
(define-foreign-type IplImage* (c-pointer "IplImage"))
(define-foreign-type CvArr* (c-pointer "CvArr"))

(define (load-image file)
  (let ((ptr (cvLoadImage file CV_LOAD_IMAGE_COLOR)))
    (set-finalizer! ptr release-image)
    (wrap-IplImage ptr)))

(define (wait-key timeout) (cvWaitKey timeout))

(define (show-image window image)
  (let ((win (if (window? window)
                 (unwrap-named-window window)
                 window))
        (img (if (IplImage? image)
                 (unwrap-IplImage image)
                 (unwrap-CvMat image))))
    (cvShowImage win img)))

(define (make-window name)
  (cvNamedWindow name CV_WINDOW_AUTOSIZE)
  (wrap-named-window name))

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

(define cvWaitKey (foreign-lambda void
                                  "cvWaitKey"
                                  int))

(define cvShowImage (foreign-lambda void
                                    "cvShowImage"
                                    nonnull-c-string
                                    CvArr*))

(define-foreign-type CvMat* (c-pointer "CvMat"))

(define (decode-image bytes)
  (wrap-IplImage
    (cvDecodeImage (unwrap-CvMat bytes) CV_LOAD_IMAGE_COLOR)))

(define cvDecodeImage (foreign-lambda IplImage*
                                    "cvDecodeImage"
                                    CvMat*
                                    int))

)
