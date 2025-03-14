!=============================================================================80
!
!> Interface to standard library math functions
!
!  https://stdlib.fortran-lang.org/page/specs/stdlib_math.html
!
!=============================================================================80

module interface_stdlib_math

  use stdlib_math, only : clip_int8, clip_int16, clip_int32, clip_int64, &
                          clip_sp, clip_dp, clip_xdp, clip_qp

  use stdlib_math, only : swap_int8, swap_int16, swap_int32, swap_int64, &
                          swap_sp, swap_dp, swap_xdp, swap_qp, &
                          swap_bitset_64, swap_bitset_large, &
                          swap_csp, swap_cdp, swap_cxdp, swap_bool, &
                          swap_str, swap_stt

  use stdlib_math, only : gcd_int8, gcd_int16, gcd_int32, gcd_int64

  use stdlib_math, only : linspace_default_1_rsp_rsp, &
                          linspace_default_1_rdp_rdp, &
                          linspace_default_1_rxdp_rxdp, &
                          linspace_default_1_rqp_rqp, &
                          linspace_default_1_csp_csp, &
                          linspace_default_1_cdp_cdp, &
                          linspace_default_1_cxdp_cxdp, &
                          linspace_default_1_cqp_cqp, &
                          linspace_n_1_rsp_rsp, &
                          linspace_n_1_rdp_rdp, &
                          linspace_n_1_rxdp_rxdp, &
                          linspace_n_1_rqp_rqp, &
                          linspace_n_1_csp_csp, &
                          linspace_n_1_cdp_cdp, &
                          linspace_n_1_cxdp_cxdp, &
                          linspace_n_1_cqp_cqp, &
                          linspace_default_1_iint8_iint8, &
                          linspace_default_1_iint16_iint16, &
                          linspace_default_1_iint32_iint32, &
                          linspace_default_1_iint64_iint64, &
                          linspace_n_1_iint8_iint8, &
                          linspace_n_1_iint16_iint16, &
                          linspace_n_1_iint32_iint32, &
                          linspace_n_1_iint64_iint64

  use stdlib_math, only : logspace_1_rsp_default, &
                          logspace_1_rdp_default, &
                          logspace_1_rxdp_default, &
                          logspace_1_rqp_default, &
                          logspace_1_csp_default, &
                          logspace_1_cdp_default, &
                          logspace_1_cxdp_default, &
                          logspace_1_cqp_default, &
                          logspace_1_iint32_default, &
                          logspace_1_rsp_n, &
                          logspace_1_rdp_n, &
                          logspace_1_rxdp_n, &
                          logspace_1_rqp_n, &
                          logspace_1_csp_n, &
                          logspace_1_cdp_n, &
                          logspace_1_cxdp_n, &
                          logspace_1_cqp_n, &
                          logspace_1_iint32_n, &
                          logspace_1_rsp_n_rbase, &
                          logspace_1_rsp_n_cbase, &
                          logspace_1_rsp_n_ibase, &
                          logspace_1_rdp_n_rbase, &
                          logspace_1_rdp_n_cbase, &
                          logspace_1_rdp_n_ibase, &
                          logspace_1_rxdp_n_rbase, &
                          logspace_1_rxdp_n_cbase, &
                          logspace_1_rxdp_n_ibase, &
                          logspace_1_rqp_n_rbase, &
                          logspace_1_rqp_n_cbase, &
                          logspace_1_rqp_n_ibase, &
                          logspace_1_csp_n_rbase, &
                          logspace_1_csp_n_cbase, &
                          logspace_1_csp_n_ibase, &
                          logspace_1_cdp_n_rbase, &
                          logspace_1_cdp_n_cbase, &
                          logspace_1_cdp_n_ibase, &
                          logspace_1_cxdp_n_rbase, &
                          logspace_1_cxdp_n_cbase, &
                          logspace_1_cxdp_n_ibase, &
                          logspace_1_cqp_n_rbase, &
                          logspace_1_cqp_n_cbase, &
                          logspace_1_cqp_n_ibase, &
                          logspace_1_iint32_n_rspbase, &
                          logspace_1_iint32_n_cspbase, &
                          logspace_1_iint32_n_rdpbase, &
                          logspace_1_iint32_n_cdpbase, &
                          logspace_1_iint32_n_cxdpbase, &
                          logspace_1_iint32_n_rqpbase, &
                          logspace_1_iint32_n_ibase

    use stdlib_math, only : arg_sp, arg_dp, arg_xdp, arg_qp
    use stdlib_math, only : argd_sp, argd_dp, argd_xdp, argd_qp
    use stdlib_math, only : argpi_sp, argpi_dp, argpi_xdp, argpi_qp
    use stdlib_math, only : deg2rad_sp, deg2rad_dp, deg2rad_xdp, deg2rad_qp
    use stdlib_math, only : rad2deg_sp, rad2deg_dp, rad2deg_xdp, rad2deg_qp

    use stdlib_math, only : is_close_rsp, is_close_rdp, &
                            is_close_rxdp, is_close_rqp, &
                            is_close_csp, is_close_cdp, &
                            is_close_cxdp, is_close_cqp

    use stdlib_math, only : diff_1_sp, diff_2_sp, &
                            diff_1_dp, diff_2_dp, &
                            diff_1_xdp, diff_2_xdp, &
                            diff_1_qp, diff_2_qp, &
                            diff_1_int8, diff_2_int8, &
                            diff_1_int16, diff_2_int16, &
                            diff_1_int32, diff_2_int32, &
                            diff_1_int64, diff_2_int64

  implicit none

!...Define scope
  private

!...Declare Class
  type :: ClassMathMethod
  contains
  !! example: clip(10, 0, 100) => 10
  !!          clip(-10, 0, 100) => 0
  !!          clip(110, 0, 100) => 100
    procedure, nopass :: clip_int8 => clip_int8
    procedure, nopass :: clip_int16 => clip_int16
    procedure, nopass :: clip_int32 => clip_int32
    procedure, nopass :: clip_int64 => clip_int64
    procedure, nopass :: clip_sp => clip_sp
    procedure, nopass :: clip_dp => clip_dp
    procedure, nopass :: clip_xdp => clip_xdp
    procedure, nopass :: clip_qp => clip_qp
    generic, public :: clip => clip_int8, &
                                clip_int16, &
                                clip_int32, &
                                clip_int64, &
                                clip_sp, &
                                clip_dp, &
                                clip_xdp, &
                                clip_qp
  !! example: swap(10, 100) => (100, 10)
  !!          swap([1.0,2.0], [4.0,5.0]) => ([4.0,5.0], [1.0,2.0])
  !!          swap("hello", "world") => ("world", "hello")
    procedure, nopass :: swap_int8 => swap_int8
    procedure, nopass :: swap_int16 => swap_int16
    procedure, nopass :: swap_int32 => swap_int32
    procedure, nopass :: swap_int64 => swap_int64
    procedure, nopass :: swap_sp => swap_sp
    procedure, nopass :: swap_dp => swap_dp
    procedure, nopass :: swap_xdp => swap_xdp
    procedure, nopass :: swap_qp => swap_qp
    procedure, nopass :: swap_bitset_64 => swap_bitset_64
    procedure, nopass :: swap_bitset_large => swap_bitset_large
    procedure, nopass :: swap_csp => swap_csp
    procedure, nopass :: swap_cdp => swap_cdp
    procedure, nopass :: swap_cxdp => swap_cxdp
    procedure, nopass :: swap_bool => swap_bool
    procedure, nopass :: swap_str => swap_str
    procedure, nopass :: swap_stt => swap_stt
    generic, public :: swap => swap_int8, &
                                swap_int16, &
                                swap_int32, &
                                swap_int64, &
                                swap_sp, &
                                swap_dp, &
                                swap_xdp, &
                                swap_qp, &
                                swap_bitset_64, &
                                swap_bitset_large, &
                                swap_csp, &
                                swap_cdp, &
                                swap_cxdp, &
                                swap_bool, &
                                swap_str, &
                                swap_stt

  !...gcd
    !> example: gcd(10, 100) => 10, gcd(18, 48) => 6
    procedure, nopass :: gcd_int8 => gcd_int8
    procedure, nopass :: gcd_int16 => gcd_int16
    procedure, nopass :: gcd_int32 => gcd_int32
    procedure, nopass :: gcd_int64 => gcd_int64
    generic, public :: gcd => gcd_int8, &
                                gcd_int16, &
                                gcd_int32, &
                                gcd_int64


    !> example: linspace(1,10,2) => [1.0, 5.0, 10.0]
    procedure, nopass :: linspace_default_1_rsp_rsp => linspace_default_1_rsp_rsp
    procedure, nopass :: linspace_default_1_rdp_rdp => linspace_default_1_rdp_rdp
    procedure, nopass :: linspace_default_1_rxdp_rxdp => linspace_default_1_rxdp_rxdp
    procedure, nopass :: linspace_default_1_rqp_rqp => linspace_default_1_rqp_rqp
    procedure, nopass :: linspace_default_1_csp_csp => linspace_default_1_csp_csp
    procedure, nopass :: linspace_default_1_cdp_cdp => linspace_default_1_cdp_cdp
    procedure, nopass :: linspace_default_1_cxdp_cxdp => linspace_default_1_cxdp_cxdp
    procedure, nopass :: linspace_default_1_cqp_cqp => linspace_default_1_cqp_cqp
    procedure, nopass :: linspace_n_1_rsp_rsp => linspace_n_1_rsp_rsp
    procedure, nopass :: linspace_n_1_rdp_rdp => linspace_n_1_rdp_rdp
    procedure, nopass :: linspace_n_1_rxdp_rxdp => linspace_n_1_rxdp_rxdp
    procedure, nopass :: linspace_n_1_rqp_rqp => linspace_n_1_rqp_rqp
    procedure, nopass :: linspace_n_1_csp_csp => linspace_n_1_csp_csp
    procedure, nopass :: linspace_n_1_cdp_cdp => linspace_n_1_cdp_cdp
    procedure, nopass :: linspace_n_1_cxdp_cxdp => linspace_n_1_cxdp_cxdp
    procedure, nopass :: linspace_n_1_cqp_cqp => linspace_n_1_cqp_cqp
    procedure, nopass :: linspace_default_1_iint8_iint8 => linspace_default_1_iint8_iint8
    procedure, nopass :: linspace_default_1_iint16_iint16 => linspace_default_1_iint16_iint16
    procedure, nopass :: linspace_default_1_iint32_iint32 => linspace_default_1_iint32_iint32
    procedure, nopass :: linspace_default_1_iint64_iint64 => linspace_default_1_iint64_iint64
    procedure, nopass :: linspace_n_1_iint8_iint8 => linspace_n_1_iint8_iint8
    procedure, nopass :: linspace_n_1_iint16_iint16 => linspace_n_1_iint16_iint16
    procedure, nopass :: linspace_n_1_iint32_iint32 => linspace_n_1_iint32_iint32
    procedure, nopass :: linspace_n_1_iint64_iint64 => linspace_n_1_iint64_iint64
    generic, public :: linspace => linspace_default_1_rsp_rsp, &
                                    linspace_default_1_rdp_rdp, &
                                    linspace_default_1_rxdp_rxdp, &
                                    linspace_default_1_rqp_rqp, &
                                    linspace_default_1_csp_csp, &
                                    linspace_default_1_cdp_cdp, &
                                    linspace_default_1_cxdp_cxdp, &
                                    linspace_default_1_cqp_cqp, &
                                    linspace_n_1_rsp_rsp, &
                                    linspace_n_1_rdp_rdp, &
                                    linspace_n_1_rxdp_rxdp, &
                                    linspace_n_1_rqp_rqp, &
                                    linspace_n_1_csp_csp, &
                                    linspace_n_1_cdp_cdp, &
                                    linspace_n_1_cxdp_cxdp, &
                                    linspace_n_1_cqp_cqp, &
                                    linspace_default_1_iint8_iint8, &
                                    linspace_default_1_iint16_iint16, &
                                    linspace_default_1_iint32_iint32, &
                                    linspace_default_1_iint64_iint64, &
                                    linspace_n_1_iint8_iint8, &
                                    linspace_n_1_iint16_iint16, &
                                    linspace_n_1_iint32_iint32, &
                                    linspace_n_1_iint64_iint64

    procedure, nopass :: logspace_1_rsp_default => logspace_1_rsp_default
    procedure, nopass :: logspace_1_rdp_default => logspace_1_rdp_default
    procedure, nopass :: logspace_1_rxdp_default => logspace_1_rxdp_default
    procedure, nopass :: logspace_1_rqp_default => logspace_1_rqp_default
    procedure, nopass :: logspace_1_csp_default => logspace_1_csp_default
    procedure, nopass :: logspace_1_cdp_default => logspace_1_cdp_default
    procedure, nopass :: logspace_1_cxdp_default => logspace_1_cxdp_default
    procedure, nopass :: logspace_1_cqp_default => logspace_1_cqp_default
    procedure, nopass :: logspace_1_iint32_default => logspace_1_iint32_default
    procedure, nopass :: logspace_1_rsp_n => logspace_1_rsp_n
    procedure, nopass :: logspace_1_rdp_n => logspace_1_rdp_n
    procedure, nopass :: logspace_1_rxdp_n => logspace_1_rxdp_n
    procedure, nopass :: logspace_1_rqp_n => logspace_1_rqp_n
    procedure, nopass :: logspace_1_csp_n => logspace_1_csp_n
    procedure, nopass :: logspace_1_cdp_n => logspace_1_cdp_n
    procedure, nopass :: logspace_1_cxdp_n => logspace_1_cxdp_n
    procedure, nopass :: logspace_1_cqp_n => logspace_1_cqp_n
    procedure, nopass :: logspace_1_iint32_n => logspace_1_iint32_n
    procedure, nopass :: logspace_1_rsp_n_rbase => logspace_1_rsp_n_rbase
    procedure, nopass :: logspace_1_rsp_n_cbase => logspace_1_rsp_n_cbase
    procedure, nopass :: logspace_1_rsp_n_ibase => logspace_1_rsp_n_ibase
    procedure, nopass :: logspace_1_rdp_n_rbase => logspace_1_rdp_n_rbase
    procedure, nopass :: logspace_1_rdp_n_cbase => logspace_1_rdp_n_cbase
    procedure, nopass :: logspace_1_rdp_n_ibase => logspace_1_rdp_n_ibase
    procedure, nopass :: logspace_1_rxdp_n_rbase => logspace_1_rxdp_n_rbase
    procedure, nopass :: logspace_1_rxdp_n_cbase => logspace_1_rxdp_n_cbase
    procedure, nopass :: logspace_1_rxdp_n_ibase => logspace_1_rxdp_n_ibase
    procedure, nopass :: logspace_1_rqp_n_rbase => logspace_1_rqp_n_rbase
    procedure, nopass :: logspace_1_rqp_n_cbase => logspace_1_rqp_n_cbase
    procedure, nopass :: logspace_1_rqp_n_ibase => logspace_1_rqp_n_ibase
    procedure, nopass :: logspace_1_csp_n_rbase => logspace_1_csp_n_rbase
    procedure, nopass :: logspace_1_csp_n_cbase => logspace_1_csp_n_cbase
    procedure, nopass :: logspace_1_csp_n_ibase => logspace_1_csp_n_ibase
    procedure, nopass :: logspace_1_cdp_n_rbase => logspace_1_cdp_n_rbase
    procedure, nopass :: logspace_1_cdp_n_cbase => logspace_1_cdp_n_cbase
    procedure, nopass :: logspace_1_cdp_n_ibase => logspace_1_cdp_n_ibase
    procedure, nopass :: logspace_1_cxdp_n_rbase => logspace_1_cxdp_n_rbase
    procedure, nopass :: logspace_1_cxdp_n_cbase => logspace_1_cxdp_n_cbase
    procedure, nopass :: logspace_1_cxdp_n_ibase => logspace_1_cxdp_n_ibase
    procedure, nopass :: logspace_1_cqp_n_rbase => logspace_1_cqp_n_rbase
    procedure, nopass :: logspace_1_cqp_n_cbase => logspace_1_cqp_n_cbase
    procedure, nopass :: logspace_1_cqp_n_ibase => logspace_1_cqp_n_ibase
    procedure, nopass :: logspace_1_iint32_n_rspbase => logspace_1_iint32_n_rspbase
    procedure, nopass :: logspace_1_iint32_n_cspbase => logspace_1_iint32_n_cspbase
    procedure, nopass :: logspace_1_iint32_n_rdpbase => logspace_1_iint32_n_rdpbase
    procedure, nopass :: logspace_1_iint32_n_cdpbase => logspace_1_iint32_n_cdpbase
    procedure, nopass :: logspace_1_iint32_n_cxdpbase => logspace_1_iint32_n_cxdpbase
    procedure, nopass :: logspace_1_iint32_n_rqpbase => logspace_1_iint32_n_rqpbase
    procedure, nopass :: logspace_1_iint32_n_ibase => logspace_1_iint32_n_ibase
    generic, public :: logspace => logspace_1_rsp_default, &
                                    logspace_1_rdp_default, &
                                    logspace_1_rxdp_default, &
                                    logspace_1_rqp_default, &
                                    logspace_1_csp_default, &
                                    logspace_1_cdp_default, &
                                    logspace_1_cxdp_default, &
                                    logspace_1_cqp_default, &
                                    logspace_1_iint32_default, &
                                    logspace_1_rsp_n, &
                                    logspace_1_rdp_n, &
                                    logspace_1_rxdp_n, &
                                    logspace_1_rqp_n, &
                                    logspace_1_csp_n, &
                                    logspace_1_cdp_n, &
                                    logspace_1_cxdp_n, &
                                    logspace_1_cqp_n, &
                                    logspace_1_iint32_n, &
                                    logspace_1_rsp_n_rbase, &
                                    logspace_1_rsp_n_cbase, &
                                    logspace_1_rsp_n_ibase, &
                                    logspace_1_rdp_n_rbase, &
                                    logspace_1_rdp_n_cbase, &
                                    logspace_1_rdp_n_ibase, &
                                    logspace_1_rxdp_n_rbase, &
                                    logspace_1_rxdp_n_cbase, &
                                    logspace_1_rxdp_n_ibase, &
                                    logspace_1_rqp_n_rbase, &
                                    logspace_1_rqp_n_cbase, &
                                    logspace_1_rqp_n_ibase, &
                                    logspace_1_csp_n_rbase, &
                                    logspace_1_csp_n_cbase, &
                                    logspace_1_csp_n_ibase, &
                                    logspace_1_cdp_n_rbase, &
                                    logspace_1_cdp_n_cbase, &
                                    logspace_1_cdp_n_ibase, &
                                    logspace_1_cxdp_n_rbase, &
                                    logspace_1_cxdp_n_cbase, &
                                    logspace_1_cxdp_n_ibase, &
                                    logspace_1_cqp_n_rbase, &
                                    logspace_1_cqp_n_cbase, &
                                    logspace_1_cqp_n_ibase, &
                                    logspace_1_iint32_n_rspbase, &
                                    logspace_1_iint32_n_cspbase, &
                                    logspace_1_iint32_n_rdpbase, &
                                    logspace_1_iint32_n_cdpbase, &
                                    logspace_1_iint32_n_cxdpbase, &
                                    logspace_1_iint32_n_rqpbase, &
                                    logspace_1_iint32_n_ibase
  !! example: arg([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])
  !!              [π/2,         0.0,        -π/2,        π]
    procedure, nopass :: arg_sp => arg_sp
    procedure, nopass :: arg_dp => arg_dp
    procedure, nopass :: arg_xdp => arg_xdp
    procedure, nopass :: arg_qp => arg_qp
    generic, public :: arg => arg_sp, &
                                arg_dp, &
                                arg_xdp, &
                                arg_qp
  !! example: argd([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])
  !!               [90°, 0°, -90°, 180°]
    procedure, nopass :: argd_sp => argd_sp
    procedure, nopass :: argd_dp => argd_dp
    procedure, nopass :: argd_xdp => argd_xdp
    procedure, nopass :: argd_qp => argd_qp
    generic, public :: argd => argd_sp, &
                                      argd_dp, &
                                      argd_xdp, &
                                      argd_qp
  !! example: argpi([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])
  !!                [0.5, 0.0, -0.5, 1.0]
    procedure, nopass :: argpi_sp => argpi_sp
    procedure, nopass :: argpi_dp => argpi_dp
    procedure, nopass :: argpi_xdp => argpi_xdp
    procedure, nopass :: argpi_qp => argpi_qp
    generic, public :: argpi => argpi_sp, &
                                argpi_dp, &
                                argpi_xdp, &
                                argpi_qp
  !! example: deg2rad(90.0)      ! 1.57508
    procedure, nopass :: deg2rad_sp => deg2rad_sp
    procedure, nopass :: deg2rad_dp => deg2rad_dp
    procedure, nopass :: deg2rad_xdp => deg2rad_xdp
    procedure, nopass :: deg2rad_qp => deg2rad_qp
    generic, public :: deg2rad => deg2rad_sp, &
                                  deg2rad_dp, &
                                  deg2rad_xdp, &
                                  deg2rad_qp
  !! example: rad2deg(PI_sp / 2.0)      ! 90.0
    procedure, nopass :: rad2deg_sp => rad2deg_sp
    procedure, nopass :: rad2deg_dp => rad2deg_dp
    procedure, nopass :: rad2deg_xdp => rad2deg_xdp
    procedure, nopass :: rad2deg_qp => rad2deg_qp
    generic, public :: rad2deg => rad2deg_sp, &
                                  rad2deg_dp, &
                                  rad2deg_xdp, &
                                  rad2deg_qp
  !! example: is_close(1.0, 1.0) => .true.
    procedure, nopass :: is_close_rsp => is_close_rsp
    procedure, nopass :: is_close_rdp => is_close_rdp
    procedure, nopass :: is_close_rxdp => is_close_rxdp
    procedure, nopass :: is_close_rqp => is_close_rqp
    procedure, nopass :: is_close_csp => is_close_csp
    procedure, nopass :: is_close_cdp => is_close_cdp
    procedure, nopass :: is_close_cxdp => is_close_cxdp
    procedure, nopass :: is_close_cqp => is_close_cqp
    generic, public :: is_close => is_close_rsp, &
                                    is_close_rdp, &
                                    is_close_rxdp, &
                                    is_close_rqp, &
                                    is_close_csp, &
                                    is_close_cdp, &
                                    is_close_cxdp, &
                                    is_close_cqp
  !! diff(array, compute_times, prepend, append)
  !! example: diff([1, 1, 2, 3, 5, 8, 13]) => [0, 1, 1, 2, 3, 5]
  !!          diff([0, 5, 15, 30, 50, 75], 2) => [5.0, 5.0, 5.0, 5.0]
  !!          diff([1, 1, 2, 3, 5, 8, 13], prepend=[0]) =>
  !!                    diff([0, 1, 1, 2, 3, 5, 8, 13]) => [1, 0, 1, 1, 2, 3, 5]
  !!          diff([1, 1, 2, 3, 5, 8, 13], append=[21]) =>
  !!                   diff([1, 1, 2, 3, 5, 8, 13, 21]) => [0, 1, 1, 2, 3, 5, 8]
    procedure, nopass :: diff_1_sp => diff_1_sp
    procedure, nopass :: diff_2_sp => diff_2_sp
    procedure, nopass :: diff_1_dp => diff_1_dp
    procedure, nopass :: diff_2_dp => diff_2_dp
    procedure, nopass :: diff_1_xdp => diff_1_xdp
    procedure, nopass :: diff_2_xdp => diff_2_xdp
    procedure, nopass :: diff_1_qp => diff_1_qp
    procedure, nopass :: diff_2_qp => diff_2_qp
    procedure, nopass :: diff_1_int8 => diff_1_int8
    procedure, nopass :: diff_2_int8 => diff_2_int8
    procedure, nopass :: diff_1_int16 => diff_1_int16
    procedure, nopass :: diff_2_int16 => diff_2_int16
    procedure, nopass :: diff_1_int32 => diff_1_int32
    procedure, nopass :: diff_2_int32 => diff_2_int32
    procedure, nopass :: diff_1_int64 => diff_1_int64
    procedure, nopass :: diff_2_int64 => diff_2_int64
    generic, public :: diff => diff_1_sp, &
                                diff_2_sp, &
                                diff_1_dp, &
                                diff_2_dp, &
                                diff_1_xdp, &
                                diff_2_xdp, &
                                diff_1_qp, &
                                diff_2_qp, &
                                diff_1_int8, &
                                diff_2_int8, &
                                diff_1_int16, &
                                diff_2_int16, &
                                diff_1_int32, &
                                diff_2_int32, &
                                diff_1_int64, &
                                diff_2_int64

    ! final :: delete_ClassMathMethod  !< Destructor
  end type ClassMathMethod

end module interface_stdlib_math
