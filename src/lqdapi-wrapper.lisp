(in-package :cl-user)
(defpackage lqdapi-wrapper
  (:use :cl
        :drakma
        :ironclad
        :cl-json
	:cl-json-web-tokens
	:yason)
  (:shadow :with-array :with-object)
  (:export :get-products :get-product :get-fiat-accounts :create-order))
(in-package :lqdapi-wrapper)

(defparameter *endpoint-url*     "https://api.liquid.com")
(defparameter *api-content-type* "application/json")

(defun get-timestamp ()
  (multiple-value-bind (time1 ms1) (sb-unix::system-real-time-values)
    (parse-integer (concatenate 'string (princ-to-string time1) (princ-to-string ms1)))))

(defun create-extra-headers (sign)
  (list (cons "X-Quoine-API-Version" "2")
	(cons "X-Quoine-Auth"        sign)
        (cons "Content-Type"         *api-content-type*)))

;ok
(defun get-public-api (path)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (url           (concatenate 'string *endpoint-url* path))
         (extra-headers (list (cons "Content-Type" *api-content-type*)
			      (cons "X-Quoine-API-Version" "2"))))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :get
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :additional-headers  extra-headers)))

;ok
(defun get-private-api (token-id secret path)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (timestamp         (get-timestamp))
	 (auth-payload      (list (cons :path path)
			          (cons :nonce timestamp)
			          (cons :token_id token-id)))
	 (json-auth-payload (json:encode-json-to-string auth-payload))
	 (sign              (cl-json-web-tokens:issue (yason:parse json-auth-payload) :secret secret :algorithm :HS256))
	 (url               (concatenate 'string *endpoint-url* path))
	 (extra-headers     (create-extra-headers sign)))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :get
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :additional-headers  extra-headers)))

(defun post-private-api (token-id secret path body)
  (let* ((drakma:*drakma-default-external-format* :utf-8)
	 (drakma:*text-content-types* '(("application" . "json")))
	 (timestamp         (get-timestamp))
	 (auth-payload      (list (cons :path path)
			          (cons :nonce timestamp)
			          (cons :token_id token-id)))
	 (json-auth-payload (json:encode-json-to-string auth-payload))
	 (sign              (cl-json-web-tokens:issue (yason:parse json-auth-payload) :secret secret :algorithm :HS256))
	 (json-body         (json:encode-json-to-string body))
	 (url               (concatenate 'string *endpoint-url* path))
	 (extra-headers     (create-extra-headers sign)))
    (drakma:http-request url
                         :user-agent          :explorer
	                 :method              :post
	                 :content-type        *api-content-type*
	                 :external-format-out :utf-8
                         :external-format-in  :utf-8
                         :content             json-body
                         :additional-headers  extra-headers)))
;ok
(defun get-fiat-accounts (token-id secret)
  (let* ((path     "/fiat_accounts"))
    (get-private-api token-id secret path)))

(defun create-order (token-id secret)
  (let* ((path     "/orders")
	 (order    (list (cons :order_type "limit")
			 (cons :product_id 5)
			 (cons :side "buy")
			 (cons :quantity "0.001")
			 (cons :price "850000.0")))
	 (body     (list (cons :order order))))
    (post-private-api token-id secret path body)))

(defun get-products ()
  (let* ((path     "/products"))
    (get-public-api path)))  

(defun get-product (id)
  (let* ((path  (concatenate 'string "/products/" id)))
    (get-public-api path)))
