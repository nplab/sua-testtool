;;;
;;; Copyright (c) 2011 Michael Tuexen
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; $Id: sua-asp-tests.scm,v 1.3 2011/03/21 23:51:58 tuexen Exp $


(define (sua-asp-aspsm-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (let ((msg (sua-wait-for-message fd sua-asp-up-message?)))
      (close fd)
      (if (= (sua-get-version msg) 1)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-aspsm-v-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the version in the common header of the
;;; received packet is 1.
;;; (sua-run-asp tester-addr (list sua-asp-inactive))


(define (sua-asp-aspsm-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((msg (sua-wait-for-message fd sua-asp-down-message?)))
      (close fd)
      (if (= (sua-get-version msg) 1)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (2ua-asp-aspsm-v-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the version in the common header of the
;;; received packet is 1.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-down))


(define (sua-asp-aspsm-v-02-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active))))
    (let ((asp-inactive (sua-wait-for-message fd sua-asp-inactive-message?)))
      (if (= (sua-get-version asp-inactive) 1)
	  (begin
	    (sua-send-message fd 0 (sua-make-asp-inactive-ack-message (sua-get-parameters asp-inactive)))
	    (sua-wait-for-message fd sua-asp-down-message?)
	    (sua-send-message fd 0 (sua-make-asp-down-ack-message))
	    (close fd)
	    sua-test-result-passed)
	  (begin 
	    (close fd)
	    sua-test-result-failed)))))
;;; (sua-asp-aspsm-v-02-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_INACTIVE with version 1.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-inactive))


(define (sua-asp-aspsm-i-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-common-header (+ sua-version 1)
						   sua-reserved
						   sua-aspsm-message-class
						   sua-aspup-ack-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-aspsm-i-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT returns an ERROR(invalid version)
;;; (sua-run-asp tester-addr (list sua-asp-inactive))


(define (sua-asp-aspsm-i-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (sua-wait-for-message fd sua-asp-down-message?)
    (sua-send-message fd 0 (sua-make-common-header (+ sua-version 1)
						     sua-reserved
						     sua-aspsm-message-class
						     sua-aspdn-ack-message-type
						     sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-aspsm-i-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT returns an ERROR(invalid version)
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-down))


(define (sua-asp-aspsm-i-02-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active))))
    (let ((asp-inactive (sua-wait-for-message fd sua-asp-inactive-message?)))
      (sua-send-message fd 0 (sua-make-asp-inactive-ack-message (sua-get-parameters asp-inactive))))
    (sua-wait-for-message fd sua-asp-down-message?)
    (sua-send-message fd 0 (sua-make-common-header (+ sua-version 1)
						     sua-reserved
						     sua-aspsm-message-class
						     sua-aspdn-ack-message-type
						     sua-common-header-length)) 
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-aspsm-i-02-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_INACTIVE with version 1.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-inactive sua-asp-down))


(define (sua-asp-aspsm-i-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
;;  FIXME: Should I send the ASPUP-ACK?
;;  (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (sua-send-message fd 0 (sua-make-message sua-aspsm-message-class
					     sua-reserved-aspsm-message-type
					     (list)))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-aspsm-i-03 tester-addr tester-port sut-addr sut-port)
;;; FIXME: Why states the ETSI document that the ASP is marked as ASP_INACTIVE
;;; This test is passed iff the SUT returns an ERROR(unsupported message type)
;;; (sua-run-asp tester-addr (list sua-asp-inactive))


(define (sua-asp-aspsm-o-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-down-ack-message))
    (let ((msg (sua-wait-for-message-with-timeout fd sua-asp-active-message? 2)))
      (close fd)
      (if (null? msg)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-aspsm-o-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT does not send an ASP_ACTIVE. FIXME.
;;; (sua-run-asp tester-addr (list sua-asp-inactive))


(define (sua-asp-aspsm-o-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-active-ack-message (list)))
    (let ((msg (sua-recv-message-with-timeout fd 2)))
      (close fd)
      (if (or (null? msg)
	      (and (sua-error-message? msg)
		   (= (sua-get-error-code-from-message msg) sua-unexpected-message-error-code))
	      (sua-asp-up-message? msg))
	  sua-test-result-passed
	  (if (sua-cldt-message? msg)
	      sua-test-result-failed
	      sua-test-result-unknown)))))
;;; (sua-asp-aspsm-o-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT does send an ERROR(unexpected message).
;;; (sua-run-asp tester-addr (list sua-asp-inactive))


(define (sua-asp-asptm-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active))))
    (close fd)
    sua-test-result-passed))
;;; (sua-asp-asptm-v-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_ACTIVE.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active))


(define (sua-asp-asptm-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (if (= (sua-get-version asp-active) 1)
	  (begin
	    (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
	    (close fd)
	    sua-test-result-passed)
	  (begin 
	    (close fd)
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-v-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_ACTIVE with version 1.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active))


(define (sua-asp-asptm-v-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active))))
    (let ((asp-inactive (sua-wait-for-message fd sua-asp-inactive-message?)))
      (if (= (sua-get-version asp-inactive) 1)
	  (begin
	    (sua-send-message fd 0 (sua-make-asp-inactive-ack-message (sua-get-parameters asp-inactive)))
	    (close fd)
	    sua-test-result-passed)
	  (begin 
	    (close fd)
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-v-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ASP_INACTIVE with version 1.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-inactive))


(define (sua-asp-asptm-v-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?))
	  (heartbeat-data (random-bytes 5000)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (sua-send-message fd 0 (sua-make-beat-message heartbeat-data))
      (let ((m (sua-wait-for-message fd (lambda (m) (or (sua-beat-ack-message? m)
							(sua-error-message? m))))))
	(close fd)
	(if (sua-beat-ack-message? m)
	    sua-test-result-passed
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-v-04 tester-addr tester-port sut-addr sut-port)
;;; The last parameter is the length the hearbeat data.
;;; This test is passed iff the SUT sends a BEAT_ACK.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-reflect-beat))


(define (sua-asp-asptm-v-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?))
	  (heartbeat-data (random-bytes 600)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (sua-send-message fd 0 (sua-make-beat-message heartbeat-data))
      (let ((m (sua-wait-for-message fd (lambda (m) (or (sua-beat-ack-message? m)
							(sua-error-message? m))))))
	(close fd)
	(if (and (sua-beat-ack-message? m)
		 (equal? (sua-make-beat-ack-message heartbeat-data) m))
	    sua-test-result-passed
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-v-05 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends a BEAT_ACK with unchanged data.
;;; This is indicated by returning true.
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-reflect-beat))


(define (sua-asp-asptm-i-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-increment-version 
			      (sua-make-asp-active-ack-message (sua-get-parameters asp-active))))
      (let ((msg (sua-wait-for-message fd sua-error-message?)))
	(close fd)
	(if (= (sua-get-error-code-from-message msg)
	       sua-invalid-version-error-code)
	    sua-test-result-passed
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-i-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(invalid version).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active))


(define (sua-asp-asptm-i-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (sua-wait-for-message fd sua-asp-inactive-message?)
      (sua-send-message fd 0 (sua-increment-version 
			      (sua-make-asp-inactive-ack-message (sua-get-parameters asp-active))))
      (let ((msg (sua-wait-for-message fd sua-error-message?)))
	(close fd)
	(if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	    sua-test-result-passed
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-i-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(invalid version).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-inactive))


(define (sua-asp-asptm-i-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-message sua-asptm-message-class
					       sua-reserved-asptm-message-type
					       (list)))
      (let ((msg (sua-wait-for-message fd sua-error-message?)))
	(close fd)
	(if (= (sua-get-error-code-from-message msg)
	       sua-unsupported-message-type-error-code)
	    sua-test-result-passed
	    sua-test-result-failed)))))
;;; (sua-asp-asptm-i-03 tester-addr tester-port sut-addr sut-port)
;;; FIXME: Why does the ETSI doucment state that the IUT is in ASP_DOWN.
;;; This test is passed iff the SUT sends an ERROR(unsupported message type).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active))


(define (sua-asp-asptm-o-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (sua-wait-for-message fd sua-asp-active-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((msg (sua-recv-message-with-timeout fd 2)))
      (close fd)
      (if (or (null? msg)
	      (and (sua-error-message? msg)
		   (= (sua-get-error-code-from-message msg) sua-unexpected-message-error-code))
	      (sua-asp-active-message? msg))
	  sua-test-result-passed
	  (if (sua-data-message? msg)
	      sua-test-result-failed
	      sua-test-result-unknown)))))
;;; (sua-asp-asptm-o-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unexpected message).
;;; FIXME: How to test the data sending?
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active))


(define (sua-asp-mtr-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (let ((m (sua-wait-for-message fd (lambda (m) (or (sua-cldt-message? m)
							(sua-daud-message? m))))))
	(if (sua-daud-message? m)
	    (begin
	      (sua-send-message fd 0 (sua-make-dava-message (sua-get-parameters m)))
	      (sua-wait-for-message fd sua-cldt-message?))))
      (close fd)
      sua-test-result-unknown)))
;;; (sua-asp-mtr-v-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends DATA including a RC.
;;; FIXME
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-send-data))


(define sua-asp-mtr-v-02 sua-asp-mtr-v-01)
;;; (sua-asp-mtr-v-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends DATA including data.
;;; FIXME
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-send-data))


(define sua-asp-mtr-v-03 sua-asp-mtr-v-01)
;;; (sua-asp-mtr-v-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends DATA in a valid stream .
;;; FIXME
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-send-data))


(define (sua-asp-mtr-i-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active))))
    (sua-send-message fd 1 (append (sua-make-common-header (+ 1 sua-version)
							   sua-reserved
							   sua-connection-less-message-class
							   sua-cldt-message-type
							   (+ sua-common-header-length
							      8 8 24 24 8 4 
							      (length sccp-test-message)))
				   (sua-make-routing-context-parameter (list tester-rc-valid))
				   (sua-make-protocol-class-parameter sua-protocol-class-0 #f)
				   (sua-make-source-address-parameter sua-route-on-ssn-and-pc-indicator 
								      sua-address-indicator-ssn-mask
								      (list (sua-make-point-code-parameter tester-pc)
									    (sua-make-subsystem-number-parameter tester-ssn)))
				   (sua-make-destination-address-parameter sua-route-on-ssn-and-pc-indicator
									   sua-address-indicator-ssn-mask
									   (list (sua-make-point-code-parameter sut-pc)
										 (sua-make-subsystem-number-parameter sut-ssn)))
				   (sua-make-sequence-control-parameter 0)
				   (sua-make-data-parameter sccp-test-message)))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-asp-mtr-i-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(invalid version).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-receive-data))


(define (sua-asp-mtr-i-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (sua-send-message fd 0 (sua-make-message sua-reserved-message-class 0 (list)))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-class-error-code)
	  sua-test-result-passed
	  sua-test-result-failed)))))
;;; (sua-asp-mtr-i-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unsupported message class).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-receive-data))


(define (sua-asp-mtr-i-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (sua-send-message fd 0 (sua-make-message sua-connection-less-message-class
					       sua-reserved-cl-message-type
					       (list)))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed)))))
;;; (sua-asp-mtr-i-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unsupported message type).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-receive-data))


(define (sua-asp-mtr-i-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-accept tester-addr tester-port)))
    (sua-wait-for-message fd sua-asp-up-message?)
    (sua-send-message fd 0 (sua-make-asp-up-ack-message))
    (let ((asp-active (sua-wait-for-message fd sua-asp-active-message?)))
      (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters asp-active)))
      (sua-send-message fd 0 (sua-make-message sua-connection-oriented-message-class
					       sua-reserved-co-message-type
					       (list)))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed)))))
;;; (sua-asp-mtr-i-04 tester-addr tester-port sut-addr sut-port)
;;; This test is passed iff the SUT sends an ERROR(unsupported message type).
;;; (sua-run-asp tester-addr (list sua-asp-inactive sua-asp-active sua-asp-receive-data))


