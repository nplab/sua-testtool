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
;;; $Id: sua-sgp-tests.scm,v 1.4 2011/03/21 22:21:46 tuexen Exp $

;;;
;;; Definition of the tests for the SGP
;;;

(define (sua-sgp-aspsm-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-aspsm-v-01 tester-addr tester-port sut-addr sut-port)
;;; The test is passed if an ASPUP-ACK is returned



(define (sua-sgp-aspsm-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (let ((msg (sua-wait-for-message fd sua-notify-message?)))
      (close fd)
      (if (and (= (sua-get-status-type-from-message msg) sua-as-state-change-status-type)
	       (= (sua-get-status-info-from-message msg) sua-as-inactive))
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-aspsm-v-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ASPUP-ACK and a NOTIFY(AS_INACTIVE)



(define (sua-sgp-aspsm-v-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-down-message))
    (sua-wait-for-message fd sua-asp-down-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-aspsm-v-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ASPDN-ACK



(define (sua-sgp-aspsm-v-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-refused-management-blocking-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-aspsm-v-04 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(Refused - Management Blocking)
;;; is returned. Of course, the ASP has to be configured appropiately at the SUT.



(define (sua-sgp-aspsm-i-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-common-header (+ sua-version 1)
						   sua-reserved
						   sua-aspsm-message-class
						   sua-aspup-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-aspsm-i-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ERROR(invalid version)



(define (sua-sgp-aspsm-i-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-common-header sua-version
						   sua-reserved
						   sua-aspsm-message-class
						   sua-reserved-aspsm-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-aspsm-i-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a ERROR(unsupported message type)



(define (sua-sgp-aspsm-i-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unexpected-message-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-aspsm-i-03 tester-addr tester-port sut-addr sut-port)
;;; This test needs clarification. FIXME.



(define (sua-sgp-aspsm-i-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-common-header sua-version
						   sua-reserved
						   sua-aspsm-message-class
						   sua-reserved-aspsm-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-aspsm-i-04 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type)



(define (sua-sgp-aspsm-o-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-aspsm-o-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPUP-ACK.



(define (sua-sgp-aspsm-o-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (if (= (sua-get-error-code-from-message msg)
	     sua-unexpected-message-error-code)
	  (begin
	    (sua-wait-for-message fd sua-asp-up-ack-message?)
	    (let ((msg (sua-wait-for-message fd sua-notify-message?)))
	      (close fd)
	      (if (and (= (sua-get-status-type-from-message msg) sua-as-state-change-status-type)
		       (= (sua-get-status-info-from-message msg) sua-as-inactive))
		  sua-test-result-passed
		  sua-test-result-failed)))
	  (begin 
	    (close fd)
	    sua-test-result-failed)))))
;;; (sua-sgp-aspsm-o-002 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unexpected message),
;;; an  ASPUP-ACK and a NOTIFY(AS_INACTIVE).



(define (sua-sgp-aspsm-o-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-down-message))
    (sua-wait-for-message fd sua-asp-down-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-aspsm-o-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPDN-ACK,



(define (sua-sgp-asptm-v-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-asptm-v-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPAC-ACK.



(define (sua-sgp-asptm-v-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (let ((msg (sua-wait-for-message fd sua-notify-message?)))
      (close fd)
      (if (and (= (sua-get-status-type-from-message msg) sua-as-state-change-status-type)
	       (= (sua-get-status-info-from-message msg) sua-as-active))
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-v-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPAC-ACK and NOTIFY(AS-ACTIVE).



(define (sua-sgp-asptm-v-03 tester-addr tester-port sut-addr sut-port rc)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message (list (sua-make-routing-context-parameter (list rc)))))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (close fd)
    sua-test-result-unknown))
;;; (sua-sgp-asptm-v-03 tester-addr tester-port sut-addr sut-port tester-rc-valid)
;;; This test is passed if there is an ASPAC-ACK contains the RC.
;;; NOTE: This test does not use the asp-active-message-parameters variable.


(define (sua-sgp-asptm-v-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-inactive-message asp-inactive-message-parameters))
    (sua-wait-for-message fd sua-asp-inactive-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-asptm-v-04 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK.



(define (sua-sgp-asptm-v-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-inactive-message asp-inactive-message-parameters))
    (sua-wait-for-message fd sua-asp-inactive-ack-message?)
    (let ((msg (sua-wait-for-message fd sua-notify-message?)))
      (close fd)
      (if (and (= (sua-get-status-type-from-message msg) sua-as-state-change-status-type)
	       (= (sua-get-status-info-from-message msg) sua-as-pending))
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-v-05 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK and NOTIFY(AS-PENDING).



(define (sua-sgp-asptm-v-06 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-beat-message (string->bytes "SUA rocks")))
    (sua-wait-for-message fd sua-beat-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-asptm-v-06 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a BEAT-ACK.



(define (sua-sgp-asptm-v-07 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (let ((value (random-bytes 13)))
      (sua-send-message fd 0  (sua-make-beat-message value))
      (let ((msg (sua-wait-for-message fd sua-beat-ack-message?)))
	(close fd)
	(if (equal? msg (sua-make-beat-ack-message value))
	    sua-test-result-passed
	    sua-test-result-failed)))))
;;; (sua-sgp-asptm-v-07 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is a BEAT-ACK with unchanged data.



(define (sua-sgp-asptm-v-08 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
  (let ((fd1 (sua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (sua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (sua-send-message fd1 0 (sua-make-asp-up-message (list (sua-make-asp-id-parameter asp-id-1))))
    (sua-wait-for-message fd1 sua-asp-up-ack-message?)
    (sua-send-message fd1 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-broadcast))))
    (sua-wait-for-message fd1 sua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (sua-send-message fd2 0 (sua-make-asp-up-message (list (sua-make-asp-id-parameter asp-id-2))))
    (sua-wait-for-message fd2 sua-asp-up-ack-message?)
    (sua-send-message fd2 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-broadcast))))
    (sua-wait-for-message fd2 sua-asp-active-ack-message?)
    ;;; Now move ASP1 to ASP-INACTIVE
    (sua-send-message fd1 0 (sua-make-asp-inactive-message (list)))
    (sua-wait-for-message fd1 sua-asp-inactive-ack-message?)
    (let ((msg (sua-wait-for-message fd1 sua-notify-message?)))
      (close fd1)
      (close fd2)
      (if (and (= (sua-get-status-type-from-message msg) sua-other-status-type)
	       (= (sua-get-status-info-from-message msg) sua-insufficient-resources))
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-v-08 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends a NOTIFY.



(define (sua-sgp-asptm-v-09 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
  (let ((fd1 (sua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (sua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-INACTIVE
    (sua-send-message fd1 0 (sua-make-asp-up-message (list (sua-make-asp-id-parameter asp-id-1))))
    (sua-wait-for-message fd1 sua-asp-up-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (sua-send-message fd2 0 (sua-make-asp-up-message (list (sua-make-asp-id-parameter asp-id-2))))
    (sua-wait-for-message fd2 sua-asp-up-ack-message?)
    (sua-send-message fd2 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-override))))
    (sua-wait-for-message fd2 sua-asp-active-ack-message?)
    ;;; Now move ASP1 to ASP-ACTIVE
    (sua-send-message fd1 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-override))))
    (sua-wait-for-message fd1 sua-asp-active-ack-message?)
    (sua-wait-for-message fd2 sua-notify-message?)
    (close fd1)
    (close fd2)
    sua-test-result-passed))
;;; (sua-sgp-asptm-v-09 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends an ASPAC-ACK and a NOTIFY.



(define sua-sgp-asptm-v-10 sua-sgp-asptm-v-09)
;;; (sua-sgp-asptm-v-10 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends an ASPAC-ACK and a NOTIFY including the ASP-ID.



(define (sua-sgp-asptm-i-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-common-header (+ sua-version 1)
						     sua-reserved
						     sua-asptm-message-class
						     sua-aspac-message-type
						     sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-version-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-i-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(invalid version).



(define (sua-sgp-asptm-i-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-broadcast))))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-traffic-mode-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-i-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported traffic mode type).
;;; NOTE: This test does not used the asp-active-message-parameters variable.



(define (sua-sgp-asptm-i-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter 4))))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-traffic-mode-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-i-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported traffic mode type).
;;; NOTE: This test does not used the asp-active-message-parameters variable.



(define (sua-sgp-asptm-i-04-help tester-addr tester-port sut-addr sut-port rc)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message (list (sua-make-routing-context-parameter (list rc)))))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-invalid-routing-context-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))

(define (sua-sgp-asptm-i-04 tester-addr tester-port sut-addr sut-port)
  (sua-sgp-asptm-i-04-help tester-addr tester-port sut-addr sut-port tester-rc-invalid))
;;; (sua-sgp-asptm-i-04 tester-addr tester-port sut-addr sut-port tester-rc-invalid)
;;; This test is passed if there is an ERROR(invalid routing context)..
;;; NOTE: This test does not use the asp-active-message-parameters variabel.



(define (sua-sgp-asptm-i-05 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-common-header sua-version
						   sua-reserved
						   sua-asptm-message-class
						   5
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-i-05 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).



(define (sua-sgp-asptm-i-06 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-common-header sua-version
						   sua-reserved
						   sua-asptm-message-class
						   5
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-i-06 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).



(define (sua-sgp-asptm-i-07 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
  (let ((fd1 (sua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (sua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (sua-send-message fd1 0 (sua-make-asp-up-message (list (sua-make-asp-id-parameter asp-id-1))))
    (sua-wait-for-message fd1 sua-asp-up-ack-message?)
    (sua-send-message fd1 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-override))))
    (sua-wait-for-message fd1 sua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (sua-send-message fd2 0 (sua-make-asp-up-message (list (sua-make-asp-id-parameter asp-id-2))))
    (sua-wait-for-message fd2 sua-asp-up-ack-message?)
    (sua-send-message fd2 0 (sua-make-asp-active-message (list (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-override))))
    (sua-wait-for-message fd2 sua-asp-active-ack-message?)
    ;;; Now fail communication to ASP1 via SHUTDOWN procedure.
    (close fd1)
    (let ((msg (sua-wait-for-message fd2 sua-notify-message?)))
      (close fd2)
      (if (and (= (sua-get-status-type-from-message msg) sua-other-status-type)
	       (= (sua-get-status-info-from-message msg) sua-asp-failure))
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-asptm-i-07 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 asp-id-1 asp-id-2)
;;; This test is passed if the SUT sends a NOTIFY(ASP-FAILURE).



(define (sua-sgp-asptm-i-08 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-inactive-message asp-inactive-message-parameters))
    (sua-wait-for-message fd sua-asp-inactive-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-asptm-i-08 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK.



(define (sua-sgp-asptm-o-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-asptm-o-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPAC-ACK.



(define (sua-sgp-asptm-o-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-inactive-message asp-inactive-message-parameters))
    (sua-wait-for-message fd sua-asp-inactive-ack-message?)
    (close fd)
    sua-test-result-passed))
;;; (sua-sgp-asptm-o-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ASPIA-ACK.



(define (m3ua-sgp-mtr-v-001 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 rc-1 rc-2 tester-pc-1 tester-pc-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message (list (m3ua-make-routing-context-parameter (list rc-1 rc-2)))))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    (sleep 10) ;;; wait for DAVA
    (do ((sls 0 (+ sls 1)))
	((= sls 16))
      (m3ua-send-message fd1 1 (m3ua-make-data-message tester-pc-1 tester-pc-2 ss7-si iut-ni iut-mp sls ss7-message data-message-parameters))
      (m3ua-wait-for-message fd2 m3ua-data-message?)
      (sleep 1))
    (close fd1)
    (close fd2)
    m3ua-test-result-unkown))
;;; (m3ua-sgp-mtr-v-001 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-rc-valid-1 tester-rc-valid-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-v-002 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
  (let ((fd1 (m3ua-connect tester-addr tester-port-1 sut-addr sut-port-1))
	(fd2 (m3ua-connect tester-addr tester-port-2 sut-addr sut-port-2)))
    ;;; Move ASP1 to ASP-ACTIVE
    (m3ua-send-message fd1 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd1 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd1 m3ua-asp-active-ack-message?)
    ;;; Move ASP2 to ASP-ACTIVE
    (m3ua-send-message fd2 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd2 m3ua-asp-up-ack-message?)
    (m3ua-send-message fd2 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd2 m3ua-asp-active-ack-message?)
    (sleep 10) ;;; wait for DAVA
    (do ((sls 0 (+ sls 1)))
	((= sls 16))
      (m3ua-send-message fd1 1 (m3ua-make-data-message tester-pc-1 tester-pc-2 ss7-si iut-ni iut-mp sls ss7-message data-message-parameters))
      (m3ua-wait-for-message fd2 m3ua-data-message?)
      (sleep 1))
    (close fd1)
    (close fd2)
    m3ua-test-result-unkown))
;;; (m3ua-sgp-asptm-v-002 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-v-002-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 1 (m3ua-make-data-message tester-pc tester-pc ss7-si iut-ni iut-mp iut-sls ss7-message data-message-parameters))
    (m3ua-send-message fd 1 (apply append (cons (m3ua-make-common-header m3ua-version
									 m3ua-reserved
									 m3ua-tfer-message-class
									 m3ua-data-message-type
									 m3ua-common-header-length)
						data-message-parameters)))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-missing-parameter-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-v-002-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed if the SUT responds with an ERROR message to the second DATA message.
;;; FIXME: This does NOT match the current ETSI test but a change request.



(define m3ua-sgp-mtr-v-003 m3ua-sgp-mtr-v-002)
;;; (m3ua-sgp-asptm-v-003 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (m3ua-sgp-mtr-v-003-alternate tester-addr tester-port sut-addr sut-port)
  (let ((fd (m3ua-connect tester-addr tester-port sut-addr sut-port)))
    (m3ua-send-message fd 0 (m3ua-make-asp-up-message asp-up-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-up-ack-message?)
    (m3ua-send-message fd 0 (m3ua-make-asp-active-message asp-active-message-parameters))
    (m3ua-wait-for-message fd m3ua-asp-active-ack-message?)
    (m3ua-send-message fd 1 (m3ua-make-data-message tester-pc tester-pc ss7-si iut-ni iut-mp iut-sls ss7-message data-message-parameters))
    (m3ua-send-message fd 0 (m3ua-make-data-message tester-pc tester-pc ss7-si iut-ni iut-mp iut-sls ss7-message data-message-parameters))
    (let ((msg (m3ua-wait-for-message fd m3ua-error-message?)))
      (close fd)
      (if (= (m3ua-get-error-code-from-message msg)
	     m3ua-invalid-stream-identifier-error-code)
	  m3ua-test-result-passed
	  m3ua-test-result-failed))))
;;; (m3ua-sgp-mtr-v-003-alternate tester-addr tester-port sut-addr sut-port)
;;; This test is passed if the SUT sends an ERROR message for the second DATA message.
;;; FIXME: This does NOT match the current ETSI test but a change request.



(define m3ua-sgp-mtr-v-004 m3ua-sgp-mtr-v-002)
;;; (m3ua-sgp-asptm-v-004 tester-addr tester-port-1 tester-port-2 sut-addr sut-port-1 sut-port-2 tester-pc-1 tester-pc-2)
;;; tester-pc-1 must be the point code of ASP corresponding to tester-addr tester-port-1 <-> sut-addr sut-port-1
;;; tester-pc-2 must be the point code of ASP corresponding to tester-addr tester-port-2 <-> sut-addr sut-port-2
;;; See ETSI document.



(define (sua-sgp-mtr-i-01 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
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
;;; (sua-sgp-mtr-i-01 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(invalid version).



(define (sua-sgp-mtr-i-02 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 0 (sua-make-common-header sua-version
						   sua-reserved
						   sua-reserved-message-class
						   sua-cldt-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-class-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-mtr-i-02 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message class).



(define (sua-sgp-mtr-i-03 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 1 (sua-make-common-header sua-version
						   sua-reserved
						   sua-connection-less-message-class
						   sua-reserved-cl-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-mtr-i-03 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).


(define (sua-sgp-mtr-i-04 tester-addr tester-port sut-addr sut-port)
  (let ((fd (sua-connect tester-addr tester-port sut-addr sut-port)))
    (sua-send-message fd 0 (sua-make-asp-up-message asp-up-message-parameters))
    (sua-wait-for-message fd sua-asp-up-ack-message?)
    (sua-send-message fd 0 (sua-make-asp-active-message asp-active-message-parameters))
    (sua-wait-for-message fd sua-asp-active-ack-message?)
    (sua-send-message fd 1 (sua-make-common-header sua-version
						   sua-reserved
						   sua-connection-oriented-message-class
						   sua-reserved-co-message-type
						   sua-common-header-length))
    (let ((msg (sua-wait-for-message fd sua-error-message?)))
      (close fd)
      (if (= (sua-get-error-code-from-message msg)
	     sua-unsupported-message-type-error-code)
	  sua-test-result-passed
	  sua-test-result-failed))))
;;; (sua-sgp-mtr-i-04 tester-addr tester-port sut-addr sut-port)
;;; This test is passed if there is an ERROR(unsupported message type).
