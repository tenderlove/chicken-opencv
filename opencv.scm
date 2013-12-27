;;;; opencv.scm
;;;; Bindings to pHash

(module opencv
  (
   make-mat
   CV_8U
   CV_8S
   CV_16U
   CV_16S
   CV_32S
   CV_32F
   CV_64F
   CV_8UC1
   CV_8UC2
   CV_8UC3
   CV_8UC4
   CV_8SC1
   CV_8SC2
   CV_8SC3
   CV_8SC4
   CV_16UC1
   CV_16UC2
   CV_16UC3
   CV_16UC4
   CV_16SC1
   CV_16SC2
   CV_16SC3
   CV_16SC4
   CV_32SC1
   CV_32SC2
   CV_32SC3
   CV_32SC4
   CV_32FC1
   CV_32FC2
   CV_32FC3
   CV_32FC4
   CV_64FC1
   CV_64FC2
   CV_64FC3
   CV_64FC4
  )

(import scheme chicken foreign)
(use lolevel)

(define-record-type CvMat
  (wrap-CvMat pointer)
  CvMat?
  (pointer unwrap-CvMat))

(foreign-declare "#include <opencv/cv.h>")

(define CV_8U (foreign-value "CV_8U" int))
(define CV_8S (foreign-value "CV_8S" int))
(define CV_16U (foreign-value "CV_16U" int))
(define CV_16S (foreign-value "CV_16S" int))
(define CV_32S (foreign-value "CV_32S" int))
(define CV_32F (foreign-value "CV_32F" int))
(define CV_64F (foreign-value "CV_64F" int))

(define CV_8UC1 (foreign-value "CV_8UC1" int))
(define CV_8UC2 (foreign-value "CV_8UC2" int))
(define CV_8UC3 (foreign-value "CV_8UC3" int))
(define CV_8UC4 (foreign-value "CV_8UC4" int))
(define CV_8SC1 (foreign-value "CV_8SC1" int))
(define CV_8SC2 (foreign-value "CV_8SC2" int))
(define CV_8SC3 (foreign-value "CV_8SC3" int))
(define CV_8SC4 (foreign-value "CV_8SC4" int))
(define CV_16UC1 (foreign-value "CV_16UC1" int))
(define CV_16UC2 (foreign-value "CV_16UC2" int))
(define CV_16UC3 (foreign-value "CV_16UC3" int))
(define CV_16UC4 (foreign-value "CV_16UC4" int))
(define CV_16SC1 (foreign-value "CV_16SC1" int))
(define CV_16SC2 (foreign-value "CV_16SC2" int))
(define CV_16SC3 (foreign-value "CV_16SC3" int))
(define CV_16SC4 (foreign-value "CV_16SC4" int))
(define CV_32SC1 (foreign-value "CV_32SC1" int))
(define CV_32SC2 (foreign-value "CV_32SC2" int))
(define CV_32SC3 (foreign-value "CV_32SC3" int))
(define CV_32SC4 (foreign-value "CV_32SC4" int))
(define CV_32FC1 (foreign-value "CV_32FC1" int))
(define CV_32FC2 (foreign-value "CV_32FC2" int))
(define CV_32FC3 (foreign-value "CV_32FC3" int))
(define CV_32FC4 (foreign-value "CV_32FC4" int))
(define CV_64FC1 (foreign-value "CV_64FC1" int))
(define CV_64FC2 (foreign-value "CV_64FC2" int))
(define CV_64FC3 (foreign-value "CV_64FC3" int))
(define CV_64FC4 (foreign-value "CV_64FC4" int))

(define-foreign-type CvMat "CvMat")
(define-foreign-type CvMat* (c-pointer "CvMat"))

(define (make-mat rows cols type)
  (let ((ptr (cvCreateMatHeader rows cols type)))
    (set-finalizer! ptr release-mat)
    (wrap-CvMat ptr)))

(define (release-mat ptr)
  (let-location ((i CvMat* ptr))
                (cvReleaseMat (location i))))

(define cvReleaseMat (foreign-lambda void
                                    "cvReleaseMat"
                                    (c-pointer CvMat*)))

(define cvCreateMatHeader (foreign-lambda CvMat*
                                    "cvCreateMatHeader"
                                    int
                                    int
                                    int))
)
