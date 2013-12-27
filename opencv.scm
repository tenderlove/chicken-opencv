;;;; opencv.scm
;;;; Bindings to pHash

(module opencv
  (
   make-mat
   make-mat-from-buffer
   u8mat-ref
   u8mat-set!
   unwrap-CvMat
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
(use lolevel srfi-4 ports)

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

(define-foreign-type CvMat* (c-pointer "CvMat"))

(define (make-mat rows cols type)
  (let ((ptr (cvCreateMat rows cols type)))
    (set-finalizer! ptr release-mat)
    (wrap-CvMat ptr)))

(define (make-8UC1mat rows cols)
  (make-mat rows cols CV_8UC1))

(define (u8vector-each-with-index cb vec)
  (let ((len (u8vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (cb (u8vector-ref vec i) i)
            (loop (+ 1 i)))))))

(define (make-mat-from-buffer buf)
  (let* ((len (string-length buf))
         (mat (make-8UC1mat 1 len))
         (bytes (with-input-from-string buf (lambda () (read-u8vector len)))))
    (u8vector-each-with-index (lambda (char col)
                                (u8mat-set! mat 0 col char)) bytes)
    mat))

(define (release-mat ptr)
  (let-location ((i CvMat* ptr))
                (cvReleaseMat (location i))))

(define cvReleaseMat (foreign-lambda void
                                    "cvReleaseMat"
                                    (c-pointer CvMat*)))

(define cvCreateMat (foreign-lambda CvMat*
                                    "cvCreateMat"
                                    int
                                    int
                                    int))

(define (u8mat-set! mat row col val)
  (u8mat-set (unwrap-CvMat mat) row col val))

(define u8mat-set (foreign-lambda* void
                                   ((CvMat* mat)
                                    (int row)
                                    (int col)
                                    (unsigned-byte val))
"CV_MAT_ELEM(*mat, char, row, col) = (char)val;"))

(define (u8mat-ref mat row col)
  (u8mat-get (unwrap-CvMat mat) row col))

(define u8mat-get (foreign-lambda* unsigned-byte
                                    ((CvMat* mat)
                                     (int row)
                                     (int col))
"C_return(CV_MAT_ELEM(*mat, char, row, col));"))

;(define cvGetSize (foreign-lambda c-pointer
;                                  "cvGetSize"
;                                  c-pointer))

)
