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
;;; $Id: sua.scm,v 1.7 2012/08/25 23:40:33 tuexen Exp $

(define sua-test-result-passed                0)
(define sua-test-result-failed                1)
(define sua-test-result-unknown               2)
(define sua-test-result-not-applicable      253)

;;; This is the IANA registered PPID for SUA in host byte order
(define sua-ppid                              4)

;;; This is the IANA registered port for SUA
(define sua-port                          14001)

;;; Constants for the message classes
(define sua-mgmt-message-class                0)
(define sua-ssnm-message-class                2)
(define sua-aspsm-message-class               3)
(define sua-asptm-message-class               4)
(define sua-connection-less-message-class     7)
(define sua-connection-oriented-message-class 8)
(define sua-rkm-message-class                 9)
(define sua-reserved-message-class           99)

;;; Constants for the message types
;;; MGMT messages
(define sua-err-message-type                  0)
(define sua-ntfy-message-type                 1)

;;; SSNM messages
(define sua-duna-message-type                 1)
(define sua-dava-message-type                 2)
(define sua-daud-message-type                 3)
(define sua-scon-message-type                 4)
(define sua-dupu-message-type                 5)
(define sua-drst-message-type                 6)

;;; ASPSM messages
(define sua-aspup-message-type                1)
(define sua-aspdn-message-type                2)
(define sua-beat-message-type                 3)
(define sua-aspup-ack-message-type            4)
(define sua-aspdn-ack-message-type            5)
(define sua-beat-ack-message-type             6)
(define sua-reserved-aspsm-message-type       7)

;;; ASPTM messages
(define sua-aspac-message-type                1)
(define sua-aspia-message-type                2)
(define sua-aspac-ack-message-type            3)
(define sua-aspia-ack-message-type            4)
(define sua-reserved-asptm-message-type       5)

;;; RKM messages
(define sua-reg-req-message-type              1)
(define sua-reg-rsp-message-type              2)
(define sua-dereg-req-message-type            3)
(define sua-dereg-rsp-message-type            4)
(define sua-reserved-rkm-message-type         5)

;;; CL messages
(define sua-cldt-message-type                 1)
(define sua-cldr-message-type                 2)
(define sua-reserved-cl-message-type          3)

;;; CO message
(define sua-core-message-type                 1)
(define sua-coak-message-type                 2)
(define sua-coref-message-type                3)
(define sua-relre-message-type                4)
(define sua-relco-message-type                5)
(define sua-resco-message-type                6)
(define sua-resre-message-type                7)
(define sua-codt-message-type                 8)
(define sua-coda-message-type                 9)
(define sua-coerr-message-type               10)
(define sua-coit-message-type                11)
(define sua-reserved-co-message-type         12)

;;; Constant for the protocol version
(define sua-version                           1)

;;; Constant for reserved
(define sua-reserved                          0)

;;;
;;; Creator functions for messages
;;;

(define (sua-make-common-header version reserved class type length)
  (append (uint8->bytes version)
	  (uint8->bytes reserved)
	  (uint8->bytes class)
	  (uint8->bytes type)
	  (uint32->bytes length)))

;;;(sua-make-common-header 1 2 3 4 5)
;;;(sua-make-common-header sua-version sua-reserved sua-connection-less-message-class sua-cldt-message-type 16)

(define (sua-increment-version l)
  (if (positive? (length l))
      (cons (+ (car l) 1) (cdr l))
      (list)))
;;;(sua-increment-version (list 1 2 3))
;;;(sua-increment-version (list))

;;;
;;; Creator functions for parameters
;;;

(define sua-parameter-header-length 4)
(define sua-common-header-length 8)
(define sua-data-parameter-header-length 16)

(define (sua-number-of-padding-bytes l)
  (remainder (- 4 (remainder l 4)) 4))
;;; (sua-number-of-padding-bytes 0)
;;; (sua-number-of-padding-bytes 1)
;;; (sua-number-of-padding-bytes 2)
;;; (sua-number-of-padding-bytes 3)

(define (sua-add-padding l)
  (+ l (sua-number-of-padding-bytes l)))
;;; (sua-add-padding 2)

(define (sua-padding data)
  (zero-bytes (sua-number-of-padding-bytes (length data))))
;;;(sua-padding (list 1 2 3 4 5))

(define (sua-make-parameter tag value)
  (append (uint16->bytes tag)
	  (uint16->bytes (+ (length value) sua-parameter-header-length))
	  value
	  (sua-padding value)))

(define (sua-make-random-parameter l)
  (sua-make-parameter (random 2^16) (random-bytes l)))
;;;(sua-make-random-parameter 10)

(define (sua-add-parameter parameter list)
  (cons parameter (remove (lambda(p) (equal? (sua-get-parameter-tag p)
					     (sua-get-parameter-tag parameter)))
			  list)))
;;;(sua-add-parameter (sua-make-info-string-parameter "Hello1") (list (sua-make-correlation-id-parameter 34)))
;;;(sua-add-parameter (sua-make-info-string-parameter "Hello1") (list (sua-make-correlation-id-parameter 34) (sua-make-info-string-parameter "Hello")))

(define (sua-make-message class type parameters)
  (append (sua-make-common-header sua-version
				  sua-reserved
				  class
				  type
				  (+ sua-common-header-length (apply + (map length parameters))))
	  (apply append parameters)))

(define sua-info-string-tag                  #x0004)
(define sua-routing-context-tag              #x0006)
(define sua-diagnostic-info-tag              #x0007)
(define sua-heartbeat-data-tag               #x0009)
(define sua-traffic-mode-type-tag            #x000b)
(define sua-error-code-tag                   #x000c)
(define sua-status-tag                       #x000d)
(define sua-asp-identifier-tag               #x0011)
(define sua-affected-point-code-tag          #x0012)
(define sua-correlation-id-tag               #x0013)
(define sua-registration-result-tag          #x0014)
(define sua-deregistration-result-tag        #x0015)
(define sua-registration-status-tag          #x0016)
(define sua-deregistration-status-tag        #x0017)
(define sua-local-routing-key-identifier-tag #x0018)

(define sua-ss7-hop-counter-tag              #x0101)
(define sua-source-address-tag               #x0102)
(define sua-destination-address-tag          #x0103)
(define sua-source-reference-number-tag      #x0104)
(define sua-destination-reference-number-tag #x0105)
(define sua-sccp-cause-tag                   #x0106)
(define sua-sequence-number-tag              #x0107)
(define sua-receive-sequence-number-tag      #x0108)
(define sua-asp-capabilities-tag             #x0109)
(define sua-credit-tag                       #x010a)
(define sua-data-tag                         #x010b)
(define sua-user-cause-tag                   #x010c)
(define sua-network-appearance-tag           #x010d)
(define sua-routing-key-tag                  #x010e)
(define sua-drn-label-tag                    #x010f)
(define sua-tid-label-tag                    #x0110)
(define sua-address-range-tag                #x0111)
(define sua-smi-tag                          #x0112)
(define sua-importance-tag                   #x0113)
(define sua-message-priority-tag             #x0114)
(define sua-protocol-class-tag               #x0115)
(define sua-sequence-control-tag             #x0116)
(define sua-segmentation-tag                 #x0117)
(define sua-congestion-level-tag             #x0118)

(define sua-global-title-tag                 #x8001)
(define sua-point-code-tag                   #x8002)
(define sua-subsystem-number-tag             #x8003)
(define sua-ipv4-address-tag                 #x8004)
(define sua-hostname-tag                     #x8005)
(define sua-ipv6-address-tag                 #x8006)

(define (sua-make-info-string-parameter string)
  (sua-make-parameter sua-info-string-tag (string->bytes string)))
;;; (sua-make-info-string-parameter "Hello")

(define (sua-make-routing-context-parameter contexts)
  (sua-make-parameter sua-routing-context-tag (apply append (map uint32->bytes contexts))))
;;; (sua-make-routing-context-parameter (list 1024))
;;; (sua-make-routing-context-parameter (list))
;;; (sua-make-routing-context-parameter (list 1024 4 5 6))

(define (sua-make-diagnostic-info-parameter info)
  (sua-make-parameter sua-diagnostic-info-tag info))
;;; (sua-make-diagnostic-info-parameter (list 1 2 3 4 5))

(define (sua-make-heartbeat-data-parameter data)
  (sua-make-parameter sua-heartbeat-data-tag data))
;;; (sua-make-heartbeat-data-parameter (string->bytes "SUA rocks"))

(define sua-traffic-mode-type-override  1)
(define sua-traffic-mode-type-loadshare 2)
(define sua-traffic-mode-type-broadcast 3)
(define sua-traffic-mode-type-invalid   4)

(define (sua-make-traffic-mode-type-parameter mode)
  (sua-make-parameter sua-traffic-mode-type-tag (uint32->bytes mode)))
;;; (sua-make-traffic-mode-type-parameter sua-traffic-mode-type-override)

(define sua-invalid-version-error-code               #x0001)
(define sua-unsupported-message-class-error-code     #x0003)
(define sua-unsupported-message-type-error-code      #x0004)
(define sua-unsupported-traffic-mode-type-error-code #x0005)
(define sua-unexpected-message-error-code            #x0006)
(define sua-protocol-error-error-code                #x0007)
(define sua-invalid-stream-identifier-error-code     #x0009)
(define sua-refused-management-blocking-error-code   #x000d)
(define sua-asp-identifier-required-error-code       #x000e)
(define sua-invalid-parameter-value-error-code       #x0011)
(define sua-parameter-field-error-error-code         #x0012)
(define sua-unexpected-parameter-error-code          #x0013)
(define sua-destination-status-unknown-error-code    #x0014)
(define sua-invalid-network-appearance-error-code    #x0015)
(define sua-missing-parameter-error-code             #x0016)
(define sua-invalid-routing-context-error-code       #x0019)
(define sua-no-configured-as-for-asp-error-code      #x001a)
(define sua-subsystem-status-unknown-error-code      #x001b)
(define sua-invalid-loadsharing-label-error-code     #x001c)

(define (sua-make-error-code-parameter code)
  (sua-make-parameter sua-error-code-tag (uint32->bytes code)))
;;; (sua-make-error-code-parameter sua-protocol-error-error-code)

(define (sua-get-error-code-from-parameter p)
  (bytes->uint32 (sua-get-parameter-value p)))
;;;(sua-get-error-code-from-parameter (sua-make-error-code-parameter sua-protocol-error-error-code))

(define sua-as-state-change-status-type 1)
(define sua-other-status-type           2)

(define sua-as-inactive                 2)
(define sua-as-active                   3)
(define sua-as-pending                  4)

(define sua-insufficient-resources      1)
(define sua-alternate-asp-active        2)
(define sua-asp-failure                 3)

(define (sua-make-status-parameter type info)
  (sua-make-parameter sua-status-tag
		      (append (uint16->bytes type)
			      (uint16->bytes info))))
;;; (sua-make-status-parameter 2 3)

(define (sua-get-status-type-from-parameter l)
  (bytes->uint16 (sua-get-parameter-value l)))
;;; (sua-get-status-type-from-parameter (sua-make-status-parameter 2 3))

(define (sua-get-status-info-from-parameter l)
  (bytes->uint16  (list-tail (sua-get-parameter-value l) 2)))
;;;  (sua-get-status-info-from-parameter (sua-make-status-parameter 2 3))

(define (sua-make-asp-id-parameter aid)
  (sua-make-parameter sua-asp-identifier-tag (uint32->bytes aid)))
;;; (sua-make-asp-id-parameter 1024)

(define (sua-make-affected-point-code-parameter mask-pc-pair-list)
  (sua-make-parameter sua-affected-point-code-tag
		      (apply append (map (lambda (x)
					   (append (uint8->bytes (car x))
						   (uint24->bytes (cadr x))))
					 mask-pc-pair-list))))
;;; (sua-make-affected-point-code-parameter (list (list 0 34) (list 255 89)))

(define (sua-make-correlation-id-parameter id)
  (sua-make-parameter sua-correlation-id-tag (uint32->bytes id)))
;;; (sua-make-correlation-id-parameter 1024)

(define (sua-make-registration-result-parameter parameterlist)
  (sua-make-parameter sua-registration-result-tag (apply append parameterlist)))
;;; (sua-make-registration-result-parameter (list (sua-make-local-routing-key-identifier-parameter 1234) (sua-make-registration-status-parameter sua-successfully-registered-reg-status) (sua-make-routing-context-parameter (list 12))))

(define (sua-make-deregistration-result-parameter parameterlist)
  (sua-make-parameter sua-deregistration-result-tag (apply append parameterlist)))
;;; (sua-make-deregistration-result-parameter (list (sua-make-routing-context-parameter (list 12)) (sua-make-deregistration-status-parameter sua-successfully-deregistered-dereg-status)))

(define sua-successfully-registered-reg-status                          0)
(define sua-error-unknown-reg-status                                    1)
(define sua-error-invalid-destination-address-reg-status                2)
(define sua-error-invalid-network-appearance-reg-status                 3)
(define sua-error-invalid-routing-key-reg-status                        4)         
(define sua-error-permission-denied-reg-status                          5)
(define sua-error-cannot-support-unique-routing-reg-status              6)
(define sua-error-routing-key-not-currently-provisioned-reg-status      7)
(define sua-error-insufficient-resources-reg-status                     8)
(define sua-error-unsupported-rk-parameter-field-reg-status             9)
(define sua-error-unsupported-invalid-traffic-handling-mode-reg-status 10)
(define sua-error-routing-key-change-refused-reg-status                11)
(define sua-error-routing-key-already-registered-req-status            12)

(define (sua-make-registration-status-parameter status)
  (sua-make-parameter sua-registration-status-tag (uint32->bytes status)))
;;; (sua-make-registration-status-parameter 123)

(define sua-successfully-deregistered-dereg-status                      0)
(define sua-error-unknown-dereg-status                                  1)
(define sua-error-invalid-routing-context-dereg-status                  2)
(define sua-error-permission-denied-dereg-status                        3)
(define sua-error-not-registered-dereg-status                           4)
(define sua-error-asp-currently-active-for-routing-context-dereg-status 5)

(define (sua-make-deregistration-status-parameter status)
  (sua-make-parameter sua-deregistration-status-tag (uint32->bytes status)))
;;; (sua-make-deregistration-status-parameter 123)

(define (sua-make-local-routing-key-identifier-parameter id)
  (sua-make-parameter sua-local-routing-key-identifier-tag (uint32->bytes id)))
;;; (sua-make-local-routing-key-identifier-parameter 234)

(define (sua-make-ss7-hop-counter-parameter counter)
  (sua-make-parameter sua-ss7-hop-counter-tag (append (uint24->bytes 0)
						      (uint8->bytes counter))))
;;; (sua-make-ss7-hop-counter-parameter 4)

(define sua-reserved-routing-indicator                  0)
(define sua-route-on-gt-indicator                       1)
(define sua-route-on-ssn-and-pc-indicator               2)
(define sua-route-on-hostname-indicator                 3)
(define sua-route-on-ssn-and-ip-address-indicator       4)

(define sua-address-indicator-ssn-mask #b0000000000000001)
(define sua-address-indicator-pc-mask  #b0000000000000010)
(define sua-address-indicator-gt-mask  #b0000000000000100)

(define (sua-make-source-address-parameter routing-indicator address-indicator parameterlist)
  (sua-make-parameter sua-source-address-tag (append (uint16->bytes routing-indicator)
						     (uint16->bytes address-indicator)
						     (apply append parameterlist))))
;;; (sua-make-source-address-parameter sua-route-on-gt-indicator sua-address-indicator-gt-mask (list))

(define (sua-make-global-title-parameter gti translation-type numbering-plan nature-of-address gt)
  (sua-make-parameter sua-global-title-tag (append (uint24->bytes 0)
						   (uint8->bytes gti)
						   (uint8->bytes (length gt))
						   (uint8->bytes translation-type)
						   (uint8->bytes numbering-plan)
						   (uint8->bytes nature-of-address)
						   gt
						   (sua-padding gt))))
;;; (sua-make-global-title-parameter 1 1 1 2 (list 3 4))

(define (sua-make-point-code-parameter pc)
  (sua-make-parameter sua-point-code-tag (uint32->bytes pc)))
;;; (sua-make-point-code-parameter 5)

(define (sua-make-subsystem-number-parameter ssn)
  (sua-make-parameter sua-subsystem-number-tag (append (uint24->bytes 0)
						 (uint8->bytes ssn))))
;;; (sua-make-subsystem-number-parameter 5)

(define (sua-make-ipv4-address-parameter addr)
  (sua-make-parameter sua-ipv4-address-tag (string->bytes addr)))
;;; (sua-make-ipv4-address-parameter "127.0.0.1")
;;; Do they mean binary format?

(define (sua-make-ipv6-address-parameter addr)
  (sua-make-parameter sua-ipv6-address-tag (string->bytes addr)))
;;; (sua-make-ipv6-address-parameter "::1")
;;; Do they mean binary format?

(define (sua-make-hostname-parameter addr)
  (sua-make-parameter sua-hostname-tag (append (string->bytes addr)
					       (list 0))))
;;; (sua-make-hostname-parameter "sctp.fh-muenster.de")

(define (sua-make-destination-address-parameter routing-indicator address-indicator parameterlist)
  (sua-make-parameter sua-destination-address-tag (append (uint16->bytes routing-indicator)
							  (uint16->bytes address-indicator)
							  (apply append parameterlist))))
;;; (sua-make-destination-address-parameter sua-route-on-gt-indicator sua-address-indicator-gt-mask (list))

(define (sua-make-source-reference-number-parameter ref)
  (sua-make-parameter sua-source-reference-number-tag (uint32->bytes ref)))
;;; (sua-make-source-reference-number-parameter 5)

(define (sua-make-destination-reference-number-parameter ref)
  (sua-make-parameter sua-destination-reference-number-tag (uint32->bytes ref)))
;;; (sua-make-destination-reference-number-parameter 5)

(define sua-return-cause-type       1)
(define sua-refusal-cause-type      2)
(define sua-release-cause-type      3)
(define sua-reset-cause-type        4)
(define sua-error-cause-type        5)

(define (sua-make-sccp-cause-parameter type value)
  (sua-make-parameter sua-sccp-cause-tag (append (uint16->bytes 0)
						 (uint8->bytes type)
						 (uint8->bytes value))))
;;; (sua-make-sccp-cause-parameter sua-release-cause-type 1)

(define (sua-make-sequnce-number-parameter rec-num m-bit sent-num)
  (sua-make-parameter sua-sequence-number-tag (append (uint16->bytes 0)
						      (uint8->bytes (+ (* 2 rec-num) m-bit))
						      (uint8->bytes sent-num))))
;;; (sua-make-sequnce-number-parameter 5 1 9)

(define (sua-make-receive-sequence-number-parameter rec-num)
  (sua-make-parameter sua-receive-sequence-number-tag (append (uint24->bytes 0)
							      (uint8->bytes (* 2 rec-num)))))
;;; (sua-make-receive-sequence-number-parameter 4)

(define sua-protocol-class-0-flag #b00000001)
(define sua-protocol-class-1-flag #b00000010)
(define sua-protocol-class-2-flag #b00000100)
(define sua-protocol-class-3-flag #b00001000)

(define sua-no-interworking         0)
(define sua-asp-interworking        1)
(define sua-sg-interworking         2)
(define sua-relay-node-interworking 3)

(define (sua-make-asp-capabilities-parameter flags interworking)
  (sua-make-parameter sua-asp-capabilities-tag (append (uint16->bytes 0)
						       (uint8->bytes flags)
						       (uint8->bytes interworking))))
;;; (sua-make-asp-capabilities-parameter (logior sua-protocol-class-1-flag sua-protocol-class-2-flag) sua-sg-interworking)

(define (sua-make-credit-parameter credit)
  (sua-make-parameter sua-credit-tag (append (uint24->bytes 24)
					     (uint8->bytes credit))))
;;; (sua-make-credit-parameter 5)

(define (sua-make-data-parameter data)
  (sua-make-parameter sua-data-tag data))
;;; (sua-make-data-parameter (list 1 2 3))

(define sua-remote-sccp-unavailable  0)
(define sua-remote-sccp-unequipped   1)
(define sua-remote-sccp-inaccessible 2)

(define (sua-make-user-cause-parameter user cause)
  (sua-make-parameter sua-user-cause-tag (append (uint16->bytes cause)
						 (uint16->bytes user))))
;;; (sua-make-user-cause-parameter 3 sua-remote-sccp-inaccessible)

(define (sua-make-network-appearance-parameter appearance)
  (sua-make-parameter sua-network-appearance-tag (uint32->bytes appearance)))
;;; (sua-make-network-appearance-parameter 3)

(define (sua-make-routing-key-parameter local-routing-key-id parameterlist)
  (sua-make-parameter sua-routing-key-tag (append (sua-make-local-routing-key-identifier-parameter local-routing-key-id)
						  (apply append parameterlist))))
;;; (sua-make-routing-key-parameter 5 (list))

(define (sua-make-drn-label-parameter start end label)
  (sua-make-parameter sua-drn-label-tag (append (uint8->bytes start)
						(uint8->bytes end)
						(uint16->bytes label))))
;;; (sua-make-drn-label-parameter 2 3 4)

(define (sua-make-tid-label-parameter start end label)
  (sua-make-parameter sua-tid-label-tag (append (uint8->bytes start)
						(uint8->bytes end)
						(uint16->bytes label))))
;;; (sua-make-tid-label-parameter 2 3 4)

(define (sua-make-address-range-parameter parameterlist)
  (sua-make-parameter sua-address-range-tag (apply append parameterlist)))
;;; (sua-make-address-range-parameter (list))

(define sua-reserved-smi   #x00)
(define sua-solitary-smi   #x01)
(define sua-duplicated-smi #x02)
(define sua-triplicated    #x03)
(define sua-quadruplicated #x04)
(define sua-unspecified    #xff)

(define (sua-make-smi-parameter smi)
  (sua-make-parameter sua-smi-tag (append (uint24->bytes 0)
					  (uint8->bytes smi))))
;;; (sua-make-smi-parameter sua-solitary-smi)

(define (sua-make-importance-parameter importance)
  (sua-make-parameter sua-importance-tag (append (uint24->bytes 0)
						 (uint8->bytes importance))))
;;; (sua-make-importance-parameter 4)

(define (sua-make-message-priority-parameter priority)
  (sua-make-parameter sua-message-priority-tag (append (uint24->bytes 0)
						       (uint8->bytes priority))))
;;; (sua-make-message-priority-parameter 4)

(define sua-protocol-class-0 0)
(define sua-protocol-class-1 1)
(define sua-protocol-class-2 2)
(define sua-protocol-class-3 3)

(define (sua-make-protocol-class-parameter class return-on-error)
  (sua-make-parameter sua-protocol-class-tag (append (uint24->bytes 0)
						     (uint8->bytes (if return-on-error
								       (logior #b10000000 class)
								       class)))))
;;; (sua-make-protocol-class-parameter sua-protocol-class-1 #f)

(define (sua-make-sequence-control-parameter seq)
  (sua-make-parameter sua-sequence-control-tag (uint32->bytes seq)))
;;; (sua-make-sequence-control-parameter 3)

(define (sua-make-segmentation-parameter first num-remaining reference)
  (sua-make-parameter sua-segmentation-tag (append (uint8->bytes (if first
								     (logior #b10000000 num-remaining)
								     num-remaining))
						   (uint24->bytes reference))))
;;; (sua-make-segmentation-parameter #t 5 1)

(define sua-no-congestion-level 0)
(define sua-congestion-level-1  1)
(define sua-congestion-level-2  2)
(define sua-congestion-level-3  3)

(define (sua-make-congestion-level-parameter level)
  (sua-make-parameter sua-congestion-level-tag (append (uint24->bytes 0)
						       (uint8->bytes level))))
;;; (sua-make-congestion-level-parameter sua-congestion-level-2)

;;;------------------------------------------------------------------
;;; Parameter Predicates
;;;------------------------------------------------------------------

(define (sua-error-code-parameter? l)
  (= (sua-get-parameter-tag l) sua-error-code-tag))

(define (sua-status-parameter? l)
  (= (sua-get-parameter-tag l) sua-status-tag))

(define (sua-routing-key-parameter? l)
  (= (sua-get-parameter-tag l) sua-routing-key-tag))

(define (sua-local-routing-key-identifier-parameter? l)
  (= (sua-get-parameter-tag l) sua-local-routing-key-identifier-tag))

(define (sua-routing-context-parameter? l)
  (= (sua-get-parameter-tag l) sua-routing-context-tag))

(define (sua-registration-result-parameter? l)
  (= (sua-get-parameter-tag l) sua-registration-result-tag))

;;;------------------------------------------------------------------
;;; Message Contructors
;;;------------------------------------------------------------------

(define (sua-make-error-message code)
  (sua-make-message sua-mgmt-message-class
		    sua-err-message-type
		    (list (sua-make-error-code-parameter code))))
;;; (sua-make-error-message sua-no-configured-as-for-asp-error-code)

(define (sua-make-notify-message type info)
  (sua-make-message sua-mgmt-message-class
		    sua-ntfy-message-type
		    (list (sua-make-status-parameter type info))))
;;; (sua-make-notify-message sua-as-state-change-status-type sua-as-inactive)

(define (sua-make-duna-message parameters)
  (sua-make-message sua-ssnm-message-class
		    sua-duna-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-duna-message (list))

(define (sua-make-dava-message parameters)
  (sua-make-message sua-ssnm-message-class
		    sua-dava-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-dava-message (list))

(define (sua-make-daud-message parameters)
  (sua-make-message sua-ssnm-message-class
		    sua-daud-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-daud-message (list))

(define (sua-make-scon-message parameters)
  (sua-make-message sua-ssnm-message-class
		    sua-scon-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-scon-message (list))

(define (sua-make-dupu-message parameters)
  (sua-make-message sua-ssnm-message-class
		    sua-dupu-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-dupu-message (list))

(define (sua-make-drst-message parameters)
  (sua-make-message sua-ssnm-message-class
		    sua-drst-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-drst-message (list))

(define (sua-make-beat-message data)
  (sua-make-message sua-aspsm-message-class
		    sua-beat-message-type
		    (list (sua-make-heartbeat-data-parameter data))))
;;; (sua-make-beat-message (string->bytes "SUA rocks"))

(define (sua-make-beat-ack-message data)
  (sua-make-message sua-aspsm-message-class
		    sua-beat-ack-message-type
		    (list (sua-make-heartbeat-data-parameter data))))
;;; (sua-make-beat-ack-message (string->bytes "SUA rocks"))

(define (sua-make-asp-up-message parameters)
  (sua-make-message sua-aspsm-message-class
		    sua-aspup-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-asp-up-message (list))

(define (sua-make-asp-down-message)
  (sua-make-message sua-aspsm-message-class
		    sua-aspdn-message-type
		    (list (sua-make-info-string-parameter "SUA rocks"))))
;;; (sua-make-asp-down-message)

(define (sua-make-asp-up-ack-message)
  (sua-make-message sua-aspsm-message-class
		    sua-aspup-ack-message-type
		    (list (sua-make-info-string-parameter "SUA rocks"))))
;;; (sua-make-asp-up-ack-message)

(define (sua-make-asp-down-ack-message)
  (sua-make-message sua-aspsm-message-class
		    sua-aspdn-ack-message-type
		    (list (sua-make-info-string-parameter "SUA rocks"))))
;;; (sua-make-asp-down-ack-message)

(define (sua-make-asp-active-message parameters)
  (sua-make-message sua-asptm-message-class
		    sua-aspac-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-asp-active-message (list (sua-make-routing-context-parameter (list 3))))

(define (sua-make-asp-active-ack-message parameters)
  (sua-make-message sua-asptm-message-class
		    sua-aspac-ack-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-asp-active-ack-message (list))

(define (sua-make-asp-inactive-message parameters)
  (sua-make-message sua-asptm-message-class
		    sua-aspia-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-asp-inactive-message (list))

(define (sua-make-asp-inactive-ack-message parameters)
  (sua-make-message sua-asptm-message-class
		    sua-aspia-ack-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-asp-inactive-ack-message (list))

(define (sua-make-reg-req-message parameters)
  (sua-make-message sua-rkm-message-class
		    sua-reg-req-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-reg-req-message (list))

(define (sua-make-reg-rsp-message parameters)
  (sua-make-message sua-rkm-message-class
		    sua-reg-rsp-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-reg-rsp-message (list))

(define (sua-make-dereg-req-message parameters)
  (sua-make-message sua-rkm-message-class
		    sua-dereg-req-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-dereg-req-message (list))

(define (sua-make-dereg-rsp-message parameters)
  (sua-make-message sua-rkm-message-class
		    sua-dereg-rsp-message-type
		    (sua-add-parameter (sua-make-info-string-parameter "SUA rocks") parameters)))
;;; (sua-make-dereg-rsp-message (list))

(define (sua-make-cldt-message parameters)
  (sua-make-message sua-connection-less-message-class
		    sua-cldt-message-type
		    parameters))
;;; (sua-make-cldt-message (list))

(define (sua-make-cldr-message parameters)
  (sua-make-message sua-connection-less-message-class
		    sua-cldr-message-type
		    parameters))
;;; (sua-make-cldr-message (list))

(define (sua-make-core-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-core-message-type
		    parameters))
;;; (sua-make-core-message (list))

(define (sua-make-coak-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-coak-message-type
		    parameters))
;;; (sua-make-coak-message (list))

(define (sua-make-coref-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-coref-message-type
		    parameters))
;;; (sua-make-coref-message (list))

(define (sua-make-relre-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-relre-message-type
		    parameters))
;;; (sua-make-relre-message (list))

(define (sua-make-relco-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-relco-message-type
		    parameters))
;;; (sua-make-relco-message (list))

(define (sua-make-resco-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-resco-message-type
		    parameters))
;;; (sua-make-resco-message (list))

(define (sua-make-resre-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-resre-message-type
		    parameters))
;;; (sua-make-resre-message (list))

(define (sua-make-codt-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-codt-message-type
		    parameters))
;;; (sua-make-codt-message (list))

(define (sua-make-coda-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-coda-message-type
		    parameters))
;;; (sua-make-core-message (list))

(define (sua-make-coerr-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-coerr-message-type
		    parameters))
;;; (sua-make-coerr-message (list))

(define (sua-make-coit-message parameters)
  (sua-make-message sua-connection-oriented-message-class
		    sua-coit-message-type
		    parameters))
;;; (sua-make-coit-message (list))

;;;
;;; General accessor functions for messages
;;;

(define (sua-get-common-header l)
  (list-head l sua-common-header-length))
;;; (sua-get-common-header (sua-make-asp-up-message (list)))

(define sua-version-offset        0)
(define sua-reserved-offset       1)
(define sua-message-class-offset  2)
(define sua-message-type-offset   3)
(define sua-message-length-offset 4)

(define (sua-get-version l)
  (bytes->uint8 (list-tail l sua-version-offset)))

;;; (define hb (sua-make-beat-message (string->bytes "SUA rocks")))
;;; (sua-get-version hb)

(define (sua-get-reserved l)
  (bytes->uint8 (list-tail l sua-reserved-offset)))
;;; (sua-get-reserved hb)

(define (sua-get-message-class l)
  (bytes->uint8 (list-tail l sua-message-class-offset)))
;;; (sua-get-message-class hb)

(define (sua-get-message-type l)
  (bytes->uint8 (list-tail l sua-message-type-offset)))
;;; (sua-get-message-type hb)

(define (sua-get-message-length l)
  (bytes->uint32 (list-tail l sua-message-length-offset)))
;;; (sua-get-message-length hb)

(define (sua-get-parameters-1 l)
  (if (>= (length l) sua-parameter-header-length)
      (let ((parameter-length (sua-add-padding (sua-get-parameter-length l))))
	(cons (list-head l parameter-length)
	      (sua-get-parameters-1 (list-tail l parameter-length))))
      (list)))

(define (sua-get-parameters-of-message l)
  (if (>= (length l) sua-common-header-length)
      (sua-get-parameters-1 (list-tail l sua-common-header-length))
      (list)))
;;; (sua-get-parameters-of-message (sua-make-beat-message (string->bytes "SUA rocks")))
;;; (sua-get-parameters-of-message (list 2 2))

(define sua-get-parameters sua-get-parameters-of-message)

(define (sua-get-parameters-of-parameter l)
  (if (>= (length l) sua-common-header-length)
      (sua-get-parameters-1 (list-tail l sua-parameter-header-length))
      (list)))
;;; (sua-get-parameters-of-parameter (sua-make-routing-key-parameter 3 (list (sua-make-local-routing-key-identifier-parameter 3) (sua-make-correlation-id-parameter 4))))

(define (sua-make-registration-result-from-routing-key key status)
  (let ((local-rk-id (bytes->uint32 (list-tail (car (filter sua-local-routing-key-identifier-parameter?
							    (sua-get-parameters-of-parameter key)))
					       sua-parameter-header-length))))
    (if (= status sua-successfully-registered-reg-status)
	(let ((routing-contexts (filter sua-routing-context-parameter? (sua-get-parameters-of-parameter key))))
	  (if (null? routing-contexts)
	      (sua-make-registration-result-parameter (list (sua-make-local-routing-key-identifier-parameter local-rk-id)
							    (sua-make-registration-status-parameter status)
							    (sua-make-routing-context-parameter (list tester-rc-valid))))
	      (let ((rc (bytes->uint32 (list-tail routing-contexts) sua-parameter-header-length)))
	      	(sua-make-registration-result-parameter (list (sua-make-local-routing-key-identifier-parameter local-rk-id)
							      (sua-make-registration-status-parameter status)
							      (sua-make-routing-context-parameter (list rc)))))))
	(sua-make-registration-result-parameter (list (sua-make-local-routing-key-identifier-parameter local-rk-id)
						      (sua-make-registration-status-parameter status)
						      (sua-make-routing-context-parameter (list 0)))))))

;;; (sua-make-registration-result-from-routing-key (sua-make-routing-key-parameter 1 (list (sua-make-local-routing-key-identifier-parameter 3) (sua-make-correlation-id-parameter 4))) 0)

(define (sua-make-reg-rsp-from-reg-req reg-req)
  (let ((routing-keys (filter sua-routing-key-parameter? (sua-get-parameters-of-message reg-req))))
    (sua-make-reg-rsp-message
     (cons (sua-make-registration-result-from-routing-key (car routing-keys) sua-successfully-registered-reg-status)
	   (map (lambda (key) (sua-make-registration-result-from-routing-key key sua-error-insufficient-resources-reg-status))
		(cdr routing-keys))))))
;;; (sua-make-reg-rsp-from-reg-req (sua-make-reg-req-message (list (sua-make-routing-key-parameter 1 (list (sua-make-local-routing-key-identifier-parameter 3) (sua-make-correlation-id-parameter 4))))))

(define (sua-make-dereg-rsp-from-dereg-req dereg-req)
  (let ((rc (bytes->uint32 (list-tail (car (filter sua-routing-context-parameter? (sua-get-parameters-of-message dereg-req)))
				      sua-parameter-header-length))))
    (sua-make-dereg-rsp-message (list (sua-make-deregistration-result-parameter
				       (list (sua-make-routing-context-parameter (list rc))
					     (sua-make-deregistration-status-parameter sua-successfully-deregistered-dereg-status)))))))
;;; (sua-make-dereg-rsp-from-dereg-req (sua-make-dereg-req-message (list (sua-make-routing-context-parameter (list 1 2 3)))))

(define (sua-make-simple-reg-rsp-message id status context)
  (sua-make-reg-rsp-message (list (sua-make-registration-result-parameter
				    (list (sua-make-local-routing-key-identifier-parameter id)
					  (sua-make-registration-status-parameter status)
					  (sua-make-routing-context-parameter (list context)))))))
;;; (sua-make-simple-reg-rsp-message 1 0 0)

(define (sua-get-routing-context-from-reg-rsp reg-rsp)
  (bytes->uint32 (list-tail (car (filter sua-routing-context-parameter?
					 (sua-get-parameters-of-parameter 
					  (car (filter sua-registration-result-parameter? (sua-get-parameters-of-message reg-rsp))))))
			    sua-parameter-header-length)))
;;; (sua-get-routing-context-from-reg-rsp (sua-make-simple-reg-rsp-message 1 2 6))

(define (sua-get-error-code-from-message msg)
  (sua-get-error-code-from-parameter (car (filter sua-error-code-parameter? (sua-get-parameters msg)))))
;;; (sua-get-error-code-from-message (sua-make-error-message sua-unexpected-message-error-code))

(define (sua-get-status-type-from-message msg)
  (sua-get-status-type-from-parameter (car (filter sua-status-parameter? (sua-get-parameters msg)))))
;;; (sua-get-status-type-from-message (sua-make-notify-message 2 3))

(define (sua-get-status-info-from-message msg)
  (sua-get-status-info-from-parameter (car (filter sua-status-parameter? (sua-get-parameters msg)))))
;;; (sua-get-status-info-from-message (sua-make-notify-message 2 3))



;;;
;;; General accessor function for parameters
;;;

(define sua-parameter-tag-offset    0)
(define sua-parameter-length-offset 2)
(define sua-parameter-value-offset  4)

(define (sua-get-parameter-tag l)
  (bytes->uint16 (list-tail l sua-parameter-tag-offset)))
;;; (sua-get-parameter-tag (sua-make-parameter 1 (list 1 2 3)))

(define (sua-get-parameter-length l)
  (bytes->uint16 (list-tail l sua-parameter-length-offset)))
;;; (sua-get-parameter-length (sua-make-parameter 1 (list 1 2 3)))

(define (sua-get-parameter-value l)
  (list-tail (list-head l (sua-get-parameter-length l))  sua-parameter-value-offset))
;;; (sua-get-parameter-value (sua-make-parameter 1 (list 1 2 3)))

(define (sua-get-parameter-padding l)
  (list-tail l (sua-get-parameter-length l)))
;;; (sua-get-parameter-padding (sua-make-parameter 1 (list  1 2 3 4)))
 

;;;
;;;  SUA helper routines
;;;

(define sua-maximum-message-length (expt 2 16))

(define (sua-connect local-addr local-port remote-addr remote-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error
	   (lambda ()
	     (bind s AF_INET (inet-aton local-addr) local-port)
	     (connect s AF_INET (inet-aton remote-addr) remote-port)
	     (if (defined? 'SCTP_NODELAY)
		 (setsockopt s IPPROTO_SCTP SCTP_NODELAY 1))
	     s)
	   (lambda (key . args)
	     (close s)))))

;;; (sua-connect "127.0.0.1" 0 "127.0.0.1" sua-port)

(define (sua-accept local-addr local-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error  
	   (lambda ()
	     (bind s AF_INET (inet-aton local-addr) local-port)
	     (listen s 1)
	     (let ((ss (car (accept s))))
	       (close s)
	       (if (defined? 'SCTP_NODELAY)
		   (setsockopt ss IPPROTO_SCTP SCTP_NODELAY 1))
	       ss))
	   (lambda (key . args)
	     (close s)))))
    

;;;(sua-accept "127.0.0.1" sua-port)

(define (sua-send-message socket stream message)
  (catch 'system-error
	 (lambda()
	   (sctp-sendmsg socket (bytes->string message) (htonl sua-ppid) stream 0 0 AF_INET INADDR_ANY 0))
	 (lambda (key . args)
	   0)))

(define (sua-recv-message socket)
  (let ((buffer (make-string sua-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((n (recv! socket buffer)))
	       (string->bytes (substring buffer 0 n))))
	   (lambda (key . args)
	     (list)))))

;;; (sua-recv-message s)
(define (sua-recv-message-with-timeout socket seconds)
  (let ((buffer (make-string sua-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((result (select (list socket) (list) (list) seconds)))
	       (if (null? (car result))
		   (list)
		   (let ((n (recv! socket buffer)))
		     (string->bytes (substring buffer 0 n))))))
	   (lambda (key . args)
	     (list)))))

;;; (sua-recv-message-with-timeout s 2)

(define (sua-wait-for-message socket predicate)
  (let ((m (sua-recv-message socket)))
    (if (or (zero? (length m)) (predicate m))
	m
	(sua-wait-for-message socket predicate))))

(define (sua-wait-for-message-with-timeout socket predicate seconds)
  (let ((m (sua-recv-message-with-timeout socket seconds)))
    (if (or (zero? (length m)) (predicate m))
	m
	(sua-wait-for-message-with-timeout socket predicate seconds))))

(define (sua-version-ok? version)
  (= version sua-version))
;;; (sua-version-ok? sua-version)
;;; (sua-version-ok? (+ sua-version 1))

(define (sua-message-class-ok? class rkm-message-class-supported?)
  (or (= class sua-mgmt-message-class)
      (= class sua-ssnm-message-class)
      (= class sua-aspsm-message-class)
      (= class sua-asptm-message-class)
      (= class sua-connection-less-message-class)
      (= class sua-connection-oriented-message-class)
      (and rkm-message-class-supported? (= class sua-rkm-message-class))))
;;; (sua-message-class-ok? sua-mgmt-message-class #t)
;;; (sua-message-class-ok? sua-rkm-message-class #t)
;;; (sua-message-class-ok? sua-rkm-message-class #f)
;;; (sua-message-class-ok? 1000)

(define (sua-message-type-ok? class type)
  (cond
    ((= class sua-mgmt-message-class)
     (or (= type sua-err-message-type)
	 (= type sua-ntfy-message-type)))
    ((= class sua-ssnm-message-class)
     (or (= type sua-duna-message-type)
	 (= type sua-dava-message-type)
	 (= type sua-daud-message-type)
	 (= type sua-scon-message-type)
	 (= type sua-dupu-message-type)
	 (= type sua-drst-message-type)))
    ((= class sua-aspsm-message-class)
     (or (= type sua-aspup-message-type)
	 (= type sua-aspdn-message-type)
	 (= type sua-beat-message-type)
	 (= type sua-aspup-ack-message-type)
	 (= type sua-aspdn-ack-message-type)
	 (= type sua-beat-ack-message-type)))
    ((= class sua-asptm-message-class)
     (or (= type sua-aspac-message-type)
	 (= type sua-aspia-message-type)
	 (= type sua-aspac-ack-message-type)
	 (= type sua-aspia-ack-message-type)))
    ((= class sua-connection-less-message-class)
     (or (= type sua-cldt-message-type)
	 (= type sua-cldr-message-type)))
    ((= class sua-connection-oriented-message-class)
     (or (= type sua-core-message-type)
	 (= type sua-coak-message-type)
	 (= type sua-coref-message-type)
	 (= type sua-relre-message-type)
	 (= type sua-relco-message-type)
	 (= type sua-resco-message-type)
	 (= type sua-resre-message-type)
	 (= type sua-codt-message-type)
	 (= type sua-coda-message-type)
	 (= type sua-coerr-message-type)
	 (= type sua-coit-message-type)))
    ((= class sua-rkm-message-class)
     (or (= type sua-reg-req-message-type)
	 (= type sua-reg-rsp-message-type)
	 (= type sua-dereg-req-message-type)
	 (= type sua-dereg-rsp-message-type)))))

;;; (sua-message-type-ok? sua-aspsm-message-class 7)

(define (sua-check-common-header fd message rkm-message-class-supported?)
  (if (not (sua-version-ok? (sua-get-version message)))
      (begin
	(sua-send-message fd 0 (sua-make-error-message sua-invalid-version-error-code))
	#f)
      (if (not (sua-message-class-ok? (sua-get-message-class message) rkm-message-class-supported?))
	  (begin
	    (sua-send-message fd 0 (sua-make-error-message sua-unsupported-message-class-error-code))
	    #f)
	  (if (not (sua-message-type-ok? (sua-get-message-class message)
					 (sua-get-message-type message)))
	      (begin
		(sua-send-message fd 0 (sua-make-error-message sua-unsupported-message-type-error-code))
		#f)
	      #t))))

(define (sua-cldt-message? message)
  (and (= (sua-get-message-class message) sua-connection-less-message-class)
       (= (sua-get-message-type message)  sua-cldt-message-type)))
;;; (sua-cldt-message? (sua-make-cldt-message (list)))
;;; (sua-cldt-message? (sua-make-asp-up-message (list)))

(define (sua-error-message? message)
  (and (= (sua-get-message-class message) sua-mgmt-message-class)
       (= (sua-get-message-type message)  sua-err-message-type)))
;;; (sua-error-message? (sua-make-error-message sua-unexpected-message-error-code))
;;; (sua-error-message? (sua-make-asp-up-message (list)))

(define (sua-notify-message? message)
  (and (= (sua-get-message-class message) sua-mgmt-message-class)
       (= (sua-get-message-type message)  sua-ntfy-message-type)))
;;; (sua-notify-message? (sua-make-notify-message sua-as-state-change-status-type sua-as-inactive))
;;; (sua-notify-message? (sua-make-asp-up-message (list)))

(define (sua-beat-message? message)
  (and (= (sua-get-message-class message) sua-aspsm-message-class)
       (= (sua-get-message-type message)  sua-beat-message-type)))
;;; (sua-beat-message? (sua-make-beat-message (list 1 2 3)))
;;; (sua-beat-message? (sua-make-asp-up-message (list)))

(define (sua-beat-ack-message? message)
  (and (= (sua-get-message-class message) sua-aspsm-message-class)
       (= (sua-get-message-type message)  sua-beat-ack-message-type)))
;;; (sua-beat-ack-message? (sua-make-beat-ack-message (list 1 2 3)))
;;; (sua-beat-ack-message? (sua-make-asp-up-message (list)))

(define (sua-asp-up-message? message)
  (and (= (sua-get-message-class message) sua-aspsm-message-class)
       (= (sua-get-message-type message)  sua-aspup-message-type)))
;;; (sua-asp-up-message? (sua-make-asp-up-message (list)))
;;; (sua-asp-up-message? (sua-make-asp-down-message))

(define (sua-asp-up-ack-message? message)
  (and (= (sua-get-message-class message) sua-aspsm-message-class)
       (= (sua-get-message-type message)  sua-aspup-ack-message-type)))
;;; (sua-asp-up-ack-message? (sua-make-asp-up-ack-message))
;;; (sua-asp-up-ack-message? (sua-make-asp-down-message))

(define (sua-asp-active-message? message)
  (and (= (sua-get-message-class message) sua-asptm-message-class)
       (= (sua-get-message-type message)  sua-aspac-message-type)))
;;; (sua-asp-active-message? (sua-make-asp-active-message (list)))
;;; (sua-asp-active-message? (sua-make-asp-down-message))

(define (sua-asp-active-ack-message? message)
  (and (= (sua-get-message-class message) sua-asptm-message-class)
       (= (sua-get-message-type message)  sua-aspac-ack-message-type)))
;;; (sua-asp-active-ack-message? (sua-make-asp-active-ack-message (list)))
;;; (sua-asp-active-ack-message? (sua-make-asp-down-message))

(define (sua-asp-down-message? message)
  (and (= (sua-get-message-class message) sua-aspsm-message-class)
       (= (sua-get-message-type message)  sua-aspdn-message-type)))
;;; (sua-asp-down-message? (sua-make-asp-down-message))
;;; (sua-asp-down-message? (sua-make-asp-up-message (list)))

(define (sua-asp-down-ack-message? message)
  (and (= (sua-get-message-class message) sua-aspsm-message-class)
       (= (sua-get-message-type message)  sua-aspdn-ack-message-type)))
;;; (sua-asp-down-ack-message? (sua-make-asp-down-ack-message))
;;; (sua-asp-down-ack-message? (sua-make-asp-up-message (list)))

(define (sua-asp-inactive-message? message)
  (and (= (sua-get-message-class message) sua-asptm-message-class)
       (= (sua-get-message-type message)  sua-aspia-message-type)))
;;; (sua-asp-inactive-message? (sua-make-asp-inactive-message (list)))
;;; (sua-asp-inactive-message? (sua-make-asp-down-message))

(define (sua-asp-inactive-ack-message? message)
  (and (= (sua-get-message-class message) sua-asptm-message-class)
       (= (sua-get-message-type message)  sua-aspia-ack-message-type)))
;;; (sua-asp-inactive-ack-message? (sua-make-asp-inactive-ack-message (list)))
;;; (sua-asp-inactive-ack-message? (sua-make-asp-down-message))

(define (sua-daud-message? message)
  (and (= (sua-get-message-class message) sua-ssnm-message-class)
       (= (sua-get-message-type message)  sua-daud-message-type)))
;;; (sua-daud-message? (sua-make-daud-message (list)))
;;; (sua-daud-message? (sua-make-asp-down-message))

(define (sua-reg-req-message? message)
  (and (= (sua-get-message-class message) sua-rkm-message-class)
       (= (sua-get-message-type message)  sua-reg-req-message-type)))
;;; (sua-reg-req-message? (sua-make-reg-req-message (list)))
;;; (sua-reg-req-message? (sua-make-asp-down-message))

(define (sua-reg-rsp-message? message)
  (and (= (sua-get-message-class message) sua-rkm-message-class)
       (= (sua-get-message-type message)  sua-reg-rsp-message-type)))
;;; (sua-reg-rsp-message? (sua-make-reg-rsp-message (list)))
;;; (sua-reg-rsp-message? (sua-make-asp-down-message))

(define (sua-dereg-req-message? message)
  (and (= (sua-get-message-class message) sua-rkm-message-class)
       (= (sua-get-message-type message)  sua-dereg-req-message-type)))
;;; (sua-dereg-req-message? (sua-make-dereg-req-message (list)))
;;; (sua-dereg-req-message? (sua-make-asp-down-message))

(define (sua-dereg-rsp-message? message)
  (and (= (sua-get-message-class message) sua-rkm-message-class)
       (= (sua-get-message-type message)  sua-dereg-rsp-message-type)))
;;; (sua-dereg-rsp-message? (sua-make-dereg-rsp-message (list)))
;;; (sua-dereg-rsp-message? (sua-make-asp-down-message))

(define sua-asp-down           0)
(define sua-asp-inactive       1)
(define sua-asp-active         2)
(define sua-asp-reflect-beat   3)
(define sua-asp-send-data      4)
(define sua-asp-receive-data   5)
(define sua-asp-send-reg-req   6)
(define sua-asp-send-dereg-req 7)

(define (sua-handle-sgp-message fd state rkm-message-class-supported?)
  (let ((message (sua-recv-message fd)))
    (if (positive? (length message))
	(if (sua-check-common-header fd message rkm-message-class-supported?)
	    (cond 
	     ((sua-beat-message? message)
	      (sua-send-message fd 0 (sua-make-message sua-aspsm-message-class
						       sua-beat-ack-message-type
						       (sua-get-parameters message)))
	      (sua-handle-sgp-message fd state rkm-message-class-supported?))

	     ((sua-asp-up-message? message)
	      (if (= state sua-asp-active)
		  (sua-send-message fd 0 (sua-make-error-message sua-unexpected-message-error-code)))
	      (sua-send-message fd 0 (sua-make-asp-up-ack-message))
	      (if (not (= state sua-asp-inactive))
		  (sua-send-message fd 0 (sua-make-notify-message sua-as-state-change-status-type
								  sua-as-inactive)))
	      (sua-handle-sgp-message fd sua-asp-inactive rkm-message-class-supported?))
	     
	     ((sua-asp-active-message? message)
	      (if (= state sua-asp-down)
		  (begin
		    (sua-send-message fd 0 (sua-make-error-message sua-unexpected-message-error-code))
		    (sua-handle-sgp-message fd sua-asp-down rkm-message-class-supported?))
		  (begin
		    (sua-send-message fd 0 (sua-make-asp-active-ack-message (sua-get-parameters message)))
		    (if (not (= state sua-asp-active))
			(sua-send-message fd 0 (sua-make-notify-message sua-as-state-change-status-type
									sua-as-active)))
		    (sua-handle-sgp-message fd sua-asp-active rkm-message-class-supported?))))
	     
	     ((sua-asp-down-message? message)
	      (sua-send-message fd 0 (sua-make-asp-down-ack-message))
	      (sua-handle-sgp-message fd sua-asp-down rkm-message-class-supported?))
	     
	     ((sua-asp-inactive-message? message)
	      (if (= state sua-asp-down)
		  (begin
		    (sua-send-message fd 0 (sua-make-asp-down-ack-message))
		    (sua-handle-sgp-message fd sua-asp-down rkm-message-class-supported?))
		  (begin
		    (sua-send-message fd 0 (sua-make-asp-inactive-ack-message (list)))
		    (if (= state sua-asp-active)
			(sua-send-message fd 0 (sua-make-notify-message sua-as-state-change-status-type
									sua-as-pending)))
		    (sua-handle-sgp-message fd sua-asp-inactive rkm-message-class-supported?))))
	     ((sua-reg-req-message? message)
	      (if (= state sua-asp-inactive)
		  (sua-send-message fd 0 (sua-make-reg-rsp-from-reg-req message))
		  (sua-send-message fd 0 (sua-make-error-message sua-unexpected-message-error-code)))
	      (sua-handle-sgp-message fd state rkm-message-class-supported?))
	     ((sua-dereg-req-message? message)
	      (sua-send-message fd 0 (sua-make-dereg-rsp-from-dereg-req message))
	      (sua-handle-sgp-message fd state rkm-message-class-supported?))
	     (else
	      (sua-send-message fd 0 (sua-make-error-message sua-unexpected-message-error-code))
	      (sua-handle-sgp-message fd state rkm-message-class-supported?)))))))

(define (sua-run-sgp port rkm-message-class-supported?)
  (let ((fd (sua-accept "0.0.0.0" port)))
    (sua-handle-sgp-message fd sua-asp-down rkm-message-class-supported?)
    (close fd)))
;;; (sua-run-sgp sua-port #t) ;;; RKM message class supported
;;; (sua-run-sgp sua-port #f) ;;; RKM message class not supported




(define (sua-perform-asp-states fd current-state states)
  (if (null? states)
      (close fd)
      (cond
	((= (car states) sua-asp-down)
 	 (sua-send-message fd 0 (sua-make-asp-down-message))
	 (let ((message (sua-recv-message fd)))
	   (if (positive? (length message))
	       (if (sua-check-common-header fd message #t)
		   (if (sua-asp-down-ack-message? message)
		       (sua-perform-asp-states fd sua-asp-down (cdr states))
		       (close fd))
		   (close fd)))
	   (close fd)))
	((= (car states) sua-asp-inactive)
	 (if (= current-state sua-asp-down)
	     (begin
	       (sua-send-message fd 0 (sua-make-asp-up-message (list)))
	       (let ((message (sua-recv-message fd)))
		 (if (positive? (length message))
		     (if (sua-check-common-header fd message #t)
			 (if (sua-asp-up-ack-message? message)
			     (sua-perform-asp-states fd sua-asp-inactive (cdr states))
			     (close fd))
			 (close fd))
		     (close fd))))
	     (begin
	       (sua-send-message fd 0 (sua-make-asp-inactive-message (list)))
	       (let ((message (sua-recv-message fd)))
		 (if (positive? (length message))
		     (if (sua-check-common-header fd message #t)
			 (if (sua-asp-inactive-ack-message? message)
			     (sua-perform-asp-states fd sua-asp-inactive (cdr states))
			     (close fd))
			 (close fd))
		     (close fd))))))
	((= (car states) sua-asp-active)
	 (sua-send-message fd 0 (sua-make-asp-active-message (list)))
	 (let ((message (sua-recv-message fd)))
	   (if (positive? (length message))
	       (if (sua-check-common-header fd message #t)
		   (if (sua-asp-active-ack-message? message)
		       (sua-perform-asp-states fd sua-asp-active (cdr states))
		       (close fd))
		   (close fd))
	       (close fd))))
	((= (car states) sua-asp-reflect-beat)
	 (let ((message (sua-recv-message fd)))
	   (if (positive? (length message))
	       (if (sua-check-common-header fd message #t)
		   (if (sua-beat-message? message)
		       (begin
			 (sua-send-message fd 0 (sua-make-beat-ack-message (sua-get-parameter-value (car (sua-get-parameters message)))))
			 (sua-perform-asp-states fd current-state (cdr states)))
		       (sua-perform-asp-states fd current-state states))
		   (close fd))
	       (close fd))))
	((= (car states) sua-asp-send-data)
	 (sua-send-message fd 1 (sua-make-cldt-message (list (sua-make-routing-context-parameter (list tester-rc-valid))
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
							     (sua-make-data-parameter sccp-test-message))))
	 (sua-perform-asp-states fd current-state (cdr states)))
	((= (car states) sua-asp-receive-data)
	 (let ((message (sua-recv-message fd)))
	   (if (positive? (length message))
	       (if (sua-check-common-header fd message #t)
		   (sua-perform-asp-states fd current-state (cdr states))
		   (close fd))
	       (close fd))))
	((= (car states) sua-asp-send-reg-req)
	 (sua-send-message fd 0 (sua-make-reg-req-message 
				 (list (sua-make-routing-key-parameter
					(list (sua-make-local-routing-key-identifier-parameter 1)
					      (sua-make-destination-point-code-parameter 2))))))
	 (let ((message (sua-recv-message fd)))
	   (if (positive? (length message))
	       (if (sua-check-common-header fd message #t)
		   (sua-perform-asp-states fd current-state (cdr states))
		   (close fd))
	       (close fd))))
	((= (car states) sua-asp-send-dereg-req)
	 (sua-send-message fd 0 (sua-make-dereg-req-message (list (sua-make-routing-context-parameter (list 1)))))
	 (let ((message (sua-recv-message fd)))
	   (if (positive? (length message))
	       (if (sua-check-common-header fd message #t)
		   (sua-perform-asp-states fd current-state (cdr states))
		   (close fd))
	       (close fd))))
	(else
	 (error 'wrong-state)))))

(define (sua-run-asp remote-addr states)
  (let ((fd (sua-connect "0.0.0.0" 0 remote-addr sua-port)))
    (sua-perform-asp-states fd sua-asp-down states)))

(define (sua-send-beats local-addr local-port remote-addr remote-port number length)
  (let ((fd (sua-connect local-addr local-port remote-addr remote-port))
	(beat-message (sua-make-beat-message (random-bytes length))))
    (dotimes (n number)
	     (sua-send-message fd 0 beat-message)
	     (sua-recv-message fd))
    (sleep 1)
    (close fd)))
;;; (sua-send-beats "192.168.1.2" sua-port "192.168.1.8" sua-port 1000 1000)
