# SUA Testtool and Testsuite
A test tool for SUA as specified in [RFC 3868](https://tools.ietf.org/html/rfc3868)
and the ETSI specification
[ETSI TS 102 143](http://www.etsi.org/deliver/etsi_ts/102100_102199/102143/01.01.01_60/ts_102143v010101p.pdf).
The tests are based on the ETSI test specification
[ETSI TS 101 592](http://www.etsi.org/deliver/etsi_ts/101500_101599/101592/01.01.01_60/ts_101592v010101p.pdf).

## Requirements
This tool uses [guile](https://www.gnu.org/software/guile/) and its extension [guile-sctp](https://github.com/nplab/guile-sctp) for SCTP.

## Supported Platforms
It runs on Unix operating systems providing kernel SCTP support:
* FreeBSD.
* Linux, using the `libsctp-dev`package.
* Mac OS X, using the [SCTP-NKE](https://github.com/sctplab/SCTP_NKE_ElCapitan) for adding kernel SCTP support.
* Solaris.
