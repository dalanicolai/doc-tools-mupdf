;; -*- lexical-binding: t; -*-
(defvar mupdf-info-commands '(mupdf-page-sizes))

(defun mupdf-info (function &optional arg)
  (interactive (if (member (file-name-extension (buffer-file-name))
                           '("pdf" "epub"))
                   (list (completing-read "Select info type: "
                                          mupdf-info-commands)
                         current-prefix-arg)
                 (user-error "Buffer file not of `pdf' or `epub' type")))
  (pp (funcall (intern-soft function))
      (when arg
        (get-buffer-create "*mupdf-info*")))
  (when arg (pop-to-buffer "*mupdf-info*")))

(defun mupdf-page-sizes (&optional file)
  (setq file (or file (buffer-file-name)))
  (pcase (file-name-extension file)

    ("epub" (let ((outdir (concat "/tmp/"
                                  (file-name-as-directory (file-name-base file)))))
              (unless (file-exists-p outdir)
                (mupdf-create-thumbs))
              (let* ((size-string (cadr (split-string
                                         (car (process-lines "file"
                                                             (concat "/tmp/"
                                                                     (file-name-as-directory (file-name-base file))
                                                                     "thumb1.png")))
                                         "\\(data, \\|, [0-9]*-bit\\)")))
                     (size-list (mapcar #'string-to-number (split-string size-string " x "))))
                (make-list (length (directory-files outdir))
                           (cons (car size-list) (cadr size-list))))))

    ("pdf" (let* ((lines (process-lines "mutool" "info" "-M"
                                        file))
                  (split-lines (if (= (length lines) 11)
                                   (mapcar (lambda (l)
                                             (split-string l " " t))
                                           lines)
                                 "Output does not have 11 lines;
the function `mupdf-page-sizes' should get modified/generalized"))
                  (pages (string-to-number (cadr (nth 5 split-lines))))
                  (box-line (nth 9 split-lines)))
             (make-list pages (cons (string-to-number (nth 5 box-line))
                                    (string-to-number (nth 6 box-line))))))))

(defun mupdf-get-image-data (page width &optional file)
  (setq file (or file (buffer-file-name)))
  (call-process "mutool" nil nil nil
                "draw"
                "-o" "/tmp/pdf-temp-img"
                "-w" (number-to-string width)
                file
                (number-to-string page))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq coding-system-for-read 'binary)
    (insert-file-contents-literally "/tmp/pdf-temp-img")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mupdf-create-thumbs (&optional file force)
  (setq file (or file (buffer-file-name)))
  (let ((outdir (concat "/tmp/" (file-name-as-directory (file-name-base file)))))
    (unless (file-exists-p outdir)
      (make-directory (concat "/tmp/" (file-name-base file))))
    (call-process "mutool" nil nil nil
                  "draw"
                  "-o" (concat "/tmp/"
                               (file-name-as-directory (file-name-base file))
                               "thumb%d.png")
                  "-w" "175"
                  file)))

(defun mupdf-structured-contents (&optional file &rest pages)
  (setq file (or file (buffer-file-name)))
  (with-temp-buffer
    (call-process "mutool" nil t nil
                  "draw"
                  "-F" "stext"
                  file)
                  ;; (when pages (mapconcat #'number-to-string pages ",")))
    (libxml-parse-xml-region (point-min) (point-max))))


