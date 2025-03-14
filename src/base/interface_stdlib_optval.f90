!=============================================================================80
!
!> Interface to optval for <optional> attribute.
!
!  https://stdlib.fortran-lang.cn/page/specs/stdlib_optval.html
!
!=============================================================================80

module interface_stdlib_optval

  use stdlib_optval, only : optval_rsp, optval_rdp, optval_rxdp, optval_rqp, &
                & optval_iint8, optval_iint16, optval_iint32, optval_iint64, &
                & optval_csp, optval_cdp, optval_cxdp, optval_cqp, optval_ll1

  implicit none

!...Define scope
  private

  public  :: ClassOptionalMethod

!...Declare local variables
  type :: ClassOptionalMethod
  contains
    procedure, nopass :: optval_rsp => optval_rsp
    procedure, nopass :: optval_rdp => optval_rdp
    procedure, nopass :: optval_rxdp => optval_rxdp
    procedure, nopass :: optval_rqp => optval_rqp
    procedure, nopass :: optval_iint8 => optval_iint8
    procedure, nopass :: optval_iint16 => optval_iint16
    procedure, nopass :: optval_iint32 => optval_iint32
    procedure, nopass :: optval_iint64 => optval_iint64
    procedure, nopass :: optval_csp => optval_csp
    procedure, nopass :: optval_cdp => optval_cdp
    procedure, nopass :: optval_cxdp => optval_cxdp
    procedure, nopass :: optval_cqp => optval_cqp
    procedure, nopass :: optval_ll1 => optval_ll1
    generic, public :: value => optval_rsp, &
                                  optval_rdp, &
                                  optval_rxdp, &
                                  optval_rqp, &
                                  optval_iint8, &
                                  optval_iint16, &
                                  optval_iint32, &
                                  optval_iint64, &
                                  optval_csp, &
                                  optval_cdp, &
                                  optval_cxdp, &
                                  optval_cqp, &
                                  optval_ll1
    ! final :: delete_ClassOptionalMethod  !< Destructor
  end type ClassOptionalMethod

contains

end module interface_stdlib_optval
