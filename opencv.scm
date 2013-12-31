;;;; opencv.scm
;;;; Bindings to pHash

(module opencv
  (
   make-mat
   make-mat-from-buffer
   u8mat-ref
   u8mat-set!
   get-size
   BGR2GRAY
   canny
   make-mem-storage
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

   CV_RETR_LIST
   CV_CHAIN_APPROX_SIMPLE

   ;; highgui
   make-window
   load-image
   decode-image
   wait-key
   show-image
   destroy-window-named
   destroy-all-windows
   unwrap-IplImage
  )

(import scheme chicken foreign)
(use lolevel srfi-4 ports)

(define-record-type CvMat
  (wrap-CvMat pointer)
  CvMat?
  (pointer unwrap-CvMat))

(define-record-type CvMemStorage
  (wrap-CvMemStorage pointer)
  CvMemStorage?
  (pointer unwrap-CvMemStorage))

(foreign-declare "#include <opencv/cv.h>")
(foreign-declare "#include <opencv/highgui.h>")

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

(define CV_BGR2GRAY (foreign-value "CV_BGR2GRAY" int))

(define CV_RETR_LIST (foreign-value "CV_RETR_LIST" int))
(define CV_CHAIN_APPROX_SIMPLE (foreign-value "CV_CHAIN_APPROX_SIMPLE" int))

(define-foreign-type CvArr* (c-pointer "CvArr"))
(define-foreign-type CvMat* (c-pointer "CvMat"))
(define-foreign-type IplImage* (c-pointer "IplImage"))
(define-foreign-type CvMemStorage* (c-pointer "CvMemStorage"))

(define (make-mat rows cols type)
  (let ((ptr (cvCreateMat rows cols type)))
    (set-finalizer! ptr release-mat)
    (wrap-CvMat ptr)))

(define (make-8UC1mat rows cols)
  (make-mat rows cols CV_8UC1))

(define (make-mem-storage size)
  (let ((ptr (cvCreateMemStorage size)))
    (set-finalizer! ptr release-mem-storage)
    (wrap-CvMemStorage ptr)))

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

(define (BGR2GRAY img)
  (let* ((src-ptr (unwrap-IplImage img))
         (size (get-size img)))
    (let ((width (car size))
          (height (cadr size)))
      (let ((dest-ptr (cvCreateImage width height 8 1)))
        (cvCvtColor src-ptr dest-ptr CV_BGR2GRAY)
        (set-finalizer! dest-ptr release-image)
        (wrap-IplImage dest-ptr)))))

(define (get-size thing)
  (let ((vec (make-u32vector 2))
        (ptr (if (CvMat? thing)
                 (unwrap-CvMat thing)
                 (unwrap-IplImage thing))))
    (let-location ((width integer 0)
                   (height integer 0))
                  (cvGetSize ptr (location width) (location height))
                  (list width height))))

(define (canny image threshold1 threshold2 apeture-size)
  (let* ((ptr (unwrap-IplImage image))
         (size (get-size image))
         (depth (img->depth ptr))
         (channels (img->nChannels ptr))
         (dest-img (cvCreateImage (car size) (cadr size) depth channels)))
    (cvCanny ptr dest-img threshold1 threshold2 apeture-size)
    (set-finalizer! dest-img release-image)
    (wrap-IplImage dest-img)))

(define cvCanny (foreign-lambda void
                                "cvCanny"
                                CvMat*
                                CvMat*
                                double
                                double
                                int))

(define img->depth (foreign-lambda* int
                                    ((IplImage* ptr))
                                    "C_return(ptr->depth);"))

(define img->nChannels (foreign-lambda* int
                                    ((IplImage* ptr))
                                    "C_return(ptr->nChannels);"))

(define (release-mem-storage ptr)
  (let-location ((i CvMemStorage* ptr))
                (cvReleaseMemStorage (location i))))

(define cvReleaseMemStorage (foreign-lambda void
                                    "cvReleaseMemStorage"
                                    (c-pointer CvMemStorage*)))

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

(define cvGetSize (foreign-lambda* void
                                   ((CvArr* ptr)
                                    (c-pointer width)
                                    (c-pointer height))
"
CvSize s = cvGetSize((CvArr*)ptr);
*((int *)width) = s.width;
*((int *)height) = s.height;"))

(define cvCvtColor (foreign-lambda void
                                  "cvCvtColor"
                                  CvArr*
                                  CvArr*
                                  int))

(define cvCreateImage (foreign-lambda* IplImage*
                                      ((int width)
                                       (int height)
                                       (int depth)
                                       (int channels))
"C_return(cvCreateImage(cvSize(width, height), depth, channels));"))

(define cvCreateMemStorage (foreign-lambda CvMemStorage*
                                           "cvCreateMemStorage"
                                           int))

;;; highgui

(define-record-type IplImage
  (wrap-IplImage pointer)
  IplImage?
  (pointer unwrap-IplImage))

(define-record-type namedWindow
  (wrap-named-window name)
  window?
  (name unwrap-named-window))

(define CV_WINDOW_AUTOSIZE (foreign-value "CV_WINDOW_AUTOSIZE" int))
(define CV_WINDOW_NORMAL (foreign-value "CV_WINDOW_NORMAL" int))
(define CV_WINDOW_OPENGL (foreign-value "CV_WINDOW_OPENGL" int))
(define CV_LOAD_IMAGE_COLOR (foreign-value "CV_LOAD_IMAGE_COLOR" int))

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
  (cvNamedWindow name CV_WINDOW_NORMAL)
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

(define cvWaitKey (foreign-lambda int
                                  "cvWaitKey"
                                  int))

(define cvShowImage (foreign-lambda void
                                    "cvShowImage"
                                    nonnull-c-string
                                    CvArr*))

(define (decode-image bytes)
  (let ((mat (if (string? bytes)
                 (make-mat-from-buffer bytes)
                 bytes)))
    (let ((ptr (cvDecodeImage (unwrap-CvMat mat) CV_LOAD_IMAGE_COLOR)))
      (set-finalizer! ptr release-image)
      (wrap-IplImage ptr))))

(define cvDecodeImage (foreign-lambda IplImage*
                                    "cvDecodeImage"
                                    CvMat*
                                    int))

)
