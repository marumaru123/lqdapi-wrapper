(in-package :cl-user)
(defpackage lqdapi-wrapper
  (:use :cl
        :drakma
        :ironclad
        :cl-json
	:cl-json-web-tokens
	:yason)
  (:shadow :with-array :with-object)
  (:export :get-products :get-product :get-fiat-accounts :create-order :get-order-book :get-an-order :get-orders :get-an-account-details))
(in-package :lqdapi-wrapper)

(defparameter *endpoint-url*     "https://api.liquid.com")
(defparameter *api-content-type* "application/json")

(defun get-timestamp ()
  (multiple-value-bind (time1 ms1) (sb-unix::system-real-time-values)
    (parse-integer (format nil "~13,,,'0a" (concatenate 'string (princ-to-string time1) (princ-to-string ms1))))))

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

(defun get-an-account-details (token-id secret currency)
  (let* ((path  (concatenate 'string "/accounts/" currency)))
    (get-private-api token-id secret path)))  

(defun get-orders (token-id secret)
  (let* ((path     "/orders"))
    (get-private-api token-id secret path)))

(defun get-an-order (token-id secret order-id)
  (let* ((path  (concatenate 'string "/orders/" order-id)))
    (get-private-api token-id secret path)))

(defun create-order (token-id secret order-type product-id side quantity price)
  (let* ((path     "/orders")
	 (order    (list (cons :order_type order-type)
			 (cons :product_id product-id)
			 (cons :side side)
			 (cons :quantity quantity)
			 (cons :price price)))
	 (body     (list (cons :order order))))
    (post-private-api token-id secret path body)))

(defun get-products ()
  (let* ((path     "/products"))
    (get-public-api path)))  

(defun get-product (id)
  (let* ((path  (concatenate 'string "/products/" id)))
    (get-public-api path)))

(defun get-order-book (id)
  (let* ((path  (concatenate 'string "/products/" id "/price_levels")))
    (get-public-api path)))
