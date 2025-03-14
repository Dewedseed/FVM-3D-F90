!=============================================================================80
!
!> Agorithm to find the value of the k-th smallest entry in an array of size N,
!!   or the index of that value and run time is O(N), which is much faster for
!!   large arrays.
!
!  https://stdlib.fortran-lang.org/page/specs/stdlib_selection.html
!
!=============================================================================80

module interface_stdlib_array

  use stdlib_selection, only : select_1_iint8_int8, &
                               select_1_iint8_int16, &
                               select_1_iint8_int32, &
                               select_1_iint8_int64, &
                               select_1_iint16_int8, &
                               select_1_iint16_int16, &
                               select_1_iint16_int32, &
                               select_1_iint16_int64, &
                               select_1_iint32_int8, &
                               select_1_iint32_int16, &
                               select_1_iint32_int32, &
                               select_1_iint32_int64, &
                               select_1_iint64_int8, &
                               select_1_iint64_int16, &
                               select_1_iint64_int32, &
                               select_1_iint64_int64, &
                               select_1_rsp_int8, &
                               select_1_rsp_int16, &
                               select_1_rsp_int32, &
                               select_1_rsp_int64, &
                               select_1_rdp_int8, &
                               select_1_rdp_int16, &
                               select_1_rdp_int32, &
                               select_1_rdp_int64, &
                               select_1_rxdp_int8, &
                               select_1_rxdp_int16, &
                               select_1_rxdp_int32, &
                               select_1_rxdp_int64, &
                               select_1_rqp_int8, &
                               select_1_rqp_int16, &
                               select_1_rqp_int32, &
                               select_1_rqp_int64

  use stdlib_selection, only : arg_select_1_iint8_int8, &
                               arg_select_1_iint8_int16, &
                               arg_select_1_iint8_int32, &
                               arg_select_1_iint8_int64, &
                               arg_select_1_iint16_int8, &
                               arg_select_1_iint16_int16, &
                               arg_select_1_iint16_int32, &
                               arg_select_1_iint16_int64, &
                               arg_select_1_iint32_int8, &
                               arg_select_1_iint32_int16, &
                               arg_select_1_iint32_int32, &
                               arg_select_1_iint32_int64, &
                               arg_select_1_iint64_int8, &
                               arg_select_1_iint64_int16, &
                               arg_select_1_iint64_int32, &
                               arg_select_1_iint64_int64, &
                               arg_select_1_rsp_int8, &
                               arg_select_1_rsp_int16, &
                               arg_select_1_rsp_int32, &
                               arg_select_1_rsp_int64, &
                               arg_select_1_rdp_int8, &
                               arg_select_1_rdp_int16, &
                               arg_select_1_rdp_int32, &
                               arg_select_1_rdp_int64, &
                               arg_select_1_rxdp_int8, &
                               arg_select_1_rxdp_int16, &
                               arg_select_1_rxdp_int32, &
                               arg_select_1_rxdp_int64, &
                               arg_select_1_rqp_int8, &
                               arg_select_1_rqp_int16, &
                               arg_select_1_rqp_int32, &
                               arg_select_1_rqp_int64
!...Interface ord_sort
  use stdlib_sorting, only : int8_ord_sort, int16_ord_sort, &
                      int32_ord_sort, int64_ord_sort, &
                      sp_ord_sort, dp_ord_sort, &
                      xdp_ord_sort, qp_ord_sort, &
                      string_type_ord_sort, char_ord_sort, &
                      bitset_64_ord_sort, bitset_large_ord_sort
!...Interface radix_sort
  use stdlib_sorting, only : int8_radix_sort, int16_radix_sort, &
                      int32_radix_sort, int64_radix_sort, &
                      sp_radix_sort, dp_radix_sort
!...Interface sort
  use stdlib_sorting, only : int8_sort, int16_sort, int32_sort, int64_sort, &
                      sp_sort, dp_sort, xdp_sort, qp_sort, &
                      string_type_sort, char_sort, &
                      bitset_64_sort, bitset_large_sort
!...Interface sort_index
  use stdlib_sorting, only : int8_sort_index_default, int16_sort_index_default, &
                      int32_sort_index_default, int64_sort_index_default, &
                      sp_sort_index_default, dp_sort_index_default, &
                      xdp_sort_index_default, qp_sort_index_default, &
                      string_type_sort_index_default, char_sort_index_default, &
                      bitset_64_sort_index_default, bitset_large_sort_index_default, &
                      int8_sort_index_low, int16_sort_index_low, &
                      int32_sort_index_low, int64_sort_index_low, &
                      sp_sort_index_low, dp_sort_index_low, &
                      xdp_sort_index_low, qp_sort_index_low, &
                      string_type_sort_index_low, char_sort_index_low, &
                      bitset_64_sort_index_low, bitset_large_sort_index_low

  implicit none

!...Define scope
  private

  public :: ClassArrayMethod

!...Declare local variables
  type :: ClassArrayMethod
  contains
  !...call select ( array, k, kth_smallest [, left, right ] )
    ! example: select( [5, 1, 4, 2, 3], 4, kth_smallest )
    !                => [1, 2, 3, 4, 5], kth_smallest = 4
    procedure, nopass :: select_1_iint8_int8 => select_1_iint8_int8
    procedure, nopass :: select_1_iint8_int16 => select_1_iint8_int16
    procedure, nopass :: select_1_iint8_int32 => select_1_iint8_int32
    procedure, nopass :: select_1_iint8_int64 => select_1_iint8_int64
    procedure, nopass :: select_1_iint16_int8 => select_1_iint16_int8
    procedure, nopass :: select_1_iint16_int16 => select_1_iint16_int16
    procedure, nopass :: select_1_iint16_int32 => select_1_iint16_int32
    procedure, nopass :: select_1_iint16_int64 => select_1_iint16_int64
    procedure, nopass :: select_1_iint32_int8 => select_1_iint32_int8
    procedure, nopass :: select_1_iint32_int16 => select_1_iint32_int16
    procedure, nopass :: select_1_iint32_int32 => select_1_iint32_int32
    procedure, nopass :: select_1_iint32_int64 => select_1_iint32_int64
    procedure, nopass :: select_1_iint64_int8 => select_1_iint64_int8
    procedure, nopass :: select_1_iint64_int16 => select_1_iint64_int16
    procedure, nopass :: select_1_iint64_int32 => select_1_iint64_int32
    procedure, nopass :: select_1_iint64_int64 => select_1_iint64_int64
    procedure, nopass :: select_1_rsp_int8 => select_1_rsp_int8
    procedure, nopass :: select_1_rsp_int16 => select_1_rsp_int16
    procedure, nopass :: select_1_rsp_int32 => select_1_rsp_int32
    procedure, nopass :: select_1_rsp_int64 => select_1_rsp_int64
    procedure, nopass :: select_1_rdp_int8 => select_1_rdp_int8
    procedure, nopass :: select_1_rdp_int16 => select_1_rdp_int16
    procedure, nopass :: select_1_rdp_int32 => select_1_rdp_int32
    procedure, nopass :: select_1_rdp_int64 => select_1_rdp_int64
    procedure, nopass :: select_1_rxdp_int8 => select_1_rxdp_int8
    procedure, nopass :: select_1_rxdp_int16 => select_1_rxdp_int16
    procedure, nopass :: select_1_rxdp_int32 => select_1_rxdp_int32
    procedure, nopass :: select_1_rxdp_int64 => select_1_rxdp_int64
    procedure, nopass :: select_1_rqp_int8 => select_1_rqp_int8
    procedure, nopass :: select_1_rqp_int16 => select_1_rqp_int16
    procedure, nopass :: select_1_rqp_int32 => select_1_rqp_int32
    procedure, nopass :: select_1_rqp_int64 => select_1_rqp_int64
    generic, public :: select => select_1_iint8_int8, &
                                  select_1_iint8_int16, &
                                  select_1_iint8_int32, &
                                  select_1_iint8_int64, &
                                  select_1_iint16_int8, &
                                  select_1_iint16_int16, &
                                  select_1_iint16_int32, &
                                  select_1_iint16_int64, &
                                  select_1_iint32_int8, &
                                  select_1_iint32_int16, &
                                  select_1_iint32_int32, &
                                  select_1_iint32_int64, &
                                  select_1_iint64_int8, &
                                  select_1_iint64_int16, &
                                  select_1_iint64_int32, &
                                  select_1_iint64_int64, &
                                  select_1_rsp_int8, &
                                  select_1_rsp_int16, &
                                  select_1_rsp_int32, &
                                  select_1_rsp_int64, &
                                  select_1_rdp_int8, &
                                  select_1_rdp_int16, &
                                  select_1_rdp_int32, &
                                  select_1_rdp_int64, &
                                  select_1_rxdp_int8, &
                                  select_1_rxdp_int16, &
                                  select_1_rxdp_int32, &
                                  select_1_rxdp_int64, &
                                  select_1_rqp_int8, &
                                  select_1_rqp_int16, &
                                  select_1_rqp_int32, &
                                  select_1_rqp_int64

  !...call arg_select ( array, indx, k, kth_smallest [, left, right ] )
    procedure, nopass :: arg_select_1_iint8_int8 => arg_select_1_iint8_int8
    procedure, nopass :: arg_select_1_iint8_int16 => arg_select_1_iint8_int16
    procedure, nopass :: arg_select_1_iint8_int32 => arg_select_1_iint8_int32
    procedure, nopass :: arg_select_1_iint8_int64 => arg_select_1_iint8_int64
    procedure, nopass :: arg_select_1_iint16_int8 => arg_select_1_iint16_int8
    procedure, nopass :: arg_select_1_iint16_int16 => arg_select_1_iint16_int16
    procedure, nopass :: arg_select_1_iint16_int32 => arg_select_1_iint16_int32
    procedure, nopass :: arg_select_1_iint16_int64 => arg_select_1_iint16_int64
    procedure, nopass :: arg_select_1_iint32_int8 => arg_select_1_iint32_int8
    procedure, nopass :: arg_select_1_iint32_int16 => arg_select_1_iint32_int16
    procedure, nopass :: arg_select_1_iint32_int32 => arg_select_1_iint32_int32
    procedure, nopass :: arg_select_1_iint32_int64 => arg_select_1_iint32_int64
    procedure, nopass :: arg_select_1_iint64_int8 => arg_select_1_iint64_int8
    procedure, nopass :: arg_select_1_iint64_int16 => arg_select_1_iint64_int16
    procedure, nopass :: arg_select_1_iint64_int32 => arg_select_1_iint64_int32
    procedure, nopass :: arg_select_1_iint64_int64 => arg_select_1_iint64_int64
    procedure, nopass :: arg_select_1_rsp_int8 => arg_select_1_rsp_int8
    procedure, nopass :: arg_select_1_rsp_int16 => arg_select_1_rsp_int16
    procedure, nopass :: arg_select_1_rsp_int32 => arg_select_1_rsp_int32
    procedure, nopass :: arg_select_1_rsp_int64 => arg_select_1_rsp_int64
    procedure, nopass :: arg_select_1_rdp_int8 => arg_select_1_rdp_int8
    procedure, nopass :: arg_select_1_rdp_int16 => arg_select_1_rdp_int16
    procedure, nopass :: arg_select_1_rdp_int32 => arg_select_1_rdp_int32
    procedure, nopass :: arg_select_1_rdp_int64 => arg_select_1_rdp_int64
    procedure, nopass :: arg_select_1_rxdp_int8 => arg_select_1_rxdp_int8
    procedure, nopass :: arg_select_1_rxdp_int16 => arg_select_1_rxdp_int16
    procedure, nopass :: arg_select_1_rxdp_int32 => arg_select_1_rxdp_int32
    procedure, nopass :: arg_select_1_rxdp_int64 => arg_select_1_rxdp_int64
    procedure, nopass :: arg_select_1_rqp_int8 => arg_select_1_rqp_int8
    procedure, nopass :: arg_select_1_rqp_int16 => arg_select_1_rqp_int16
    procedure, nopass :: arg_select_1_rqp_int32 => arg_select_1_rqp_int32
    procedure, nopass :: arg_select_1_rqp_int64 => arg_select_1_rqp_int64
    generic, public :: arg_select => arg_select_1_iint8_int8, &
                                      arg_select_1_iint8_int16, &
                                      arg_select_1_iint8_int32, &
                                      arg_select_1_iint8_int64, &
                                      arg_select_1_iint16_int8, &
                                      arg_select_1_iint16_int16, &
                                      arg_select_1_iint16_int32, &
                                      arg_select_1_iint16_int64, &
                                      arg_select_1_iint32_int8, &
                                      arg_select_1_iint32_int16, &
                                      arg_select_1_iint32_int32, &
                                      arg_select_1_iint32_int64, &
                                      arg_select_1_iint64_int8, &
                                      arg_select_1_iint64_int16, &
                                      arg_select_1_iint64_int32, &
                                      arg_select_1_iint64_int64, &
                                      arg_select_1_rsp_int8, &
                                      arg_select_1_rsp_int16, &
                                      arg_select_1_rsp_int32, &
                                      arg_select_1_rsp_int64, &
                                      arg_select_1_rdp_int8, &
                                      arg_select_1_rdp_int16, &
                                      arg_select_1_rdp_int32, &
                                      arg_select_1_rdp_int64, &
                                      arg_select_1_rxdp_int8, &
                                      arg_select_1_rxdp_int16, &
                                      arg_select_1_rxdp_int32, &
                                      arg_select_1_rxdp_int64, &
                                      arg_select_1_rqp_int8, &
                                      arg_select_1_rqp_int16, &
                                      arg_select_1_rqp_int32, &
                                      arg_select_1_rqp_int64

  !...call ord_sort( array, work, reverse )
    ! O(N) performance on uniformly increasing or decreasing data?
    procedure, nopass :: int8_ord_sort => int8_ord_sort
    procedure, nopass :: int16_ord_sort => int16_ord_sort
    procedure, nopass :: int32_ord_sort => int32_ord_sort
    procedure, nopass :: int64_ord_sort => int64_ord_sort
    procedure, nopass :: sp_ord_sort => sp_ord_sort
    procedure, nopass :: dp_ord_sort => dp_ord_sort
    procedure, nopass :: xdp_ord_sort => xdp_ord_sort
    procedure, nopass :: qp_ord_sort => qp_ord_sort
    procedure, nopass :: string_type_ord_sort => string_type_ord_sort
    procedure, nopass :: char_ord_sort => char_ord_sort
    procedure, nopass :: bitset_64_ord_sort => bitset_64_ord_sort
    procedure, nopass :: bitset_large_ord_sort => bitset_large_ord_sort
    generic, public :: ord_sort => int8_ord_sort, &
                                    int16_ord_sort, &
                                    int32_ord_sort, &
                                    int64_ord_sort, &
                                    sp_ord_sort, &
                                    dp_ord_sort, &
                                    xdp_ord_sort, &
                                    qp_ord_sort, &
                                    string_type_ord_sort, &
                                    char_ord_sort, &
                                    bitset_64_ord_sort, &
                                    bitset_large_ord_sort

  !...call radix_sort(array, work, reverse)
    ! works for fixed width data
    ! always of O(N) runtime performance for any input data
    procedure, nopass :: int8_radix_sort => int8_radix_sort
    procedure, nopass :: int16_radix_sort => int16_radix_sort
    procedure, nopass :: int32_radix_sort => int32_radix_sort
    procedure, nopass :: int64_radix_sort => int64_radix_sort
    procedure, nopass :: sp_radix_sort => sp_radix_sort
    procedure, nopass :: dp_radix_sort => dp_radix_sort
    generic, public :: radix_sort => int8_radix_sort, &
                                      int16_radix_sort, &
                                      int32_radix_sort, &
                                      int64_radix_sort, &
                                      sp_radix_sort, &
                                      dp_radix_sort

  !...call sort( array, reverse )
    ! runtime performance is always O(NLn(N))
    ! fast on randomly ordered data
    procedure, nopass :: int8_sort => int8_sort
    procedure, nopass :: int16_sort => int16_sort
    procedure, nopass :: int32_sort => int32_sort
    procedure, nopass :: int64_sort => int64_sort
    procedure, nopass :: sp_sort => sp_sort
    procedure, nopass :: dp_sort => dp_sort
    procedure, nopass :: xdp_sort => xdp_sort
    procedure, nopass :: qp_sort => qp_sort
    procedure, nopass :: string_type_sort => string_type_sort
    procedure, nopass :: char_sort => char_sort
    procedure, nopass :: bitset_64_sort => bitset_64_sort
    procedure, nopass :: bitset_large_sort => bitset_large_sort
    generic, public :: sort => int8_sort, &
                                int16_sort, &
                                int32_sort, &
                                int64_sort, &
                                sp_sort, &
                                dp_sort, &
                                xdp_sort, &
                                qp_sort, &
                                string_type_sort, &
                                char_sort, &
                                bitset_64_sort, &
                                bitset_large_sort

  !...call sort_index( array, index, work, iwork, reverse )
    ! For multiple related rank 1 arrays, higher rank arrays, or arrays of derived types
    procedure, nopass :: int8_sort_index_default
    procedure, nopass :: int16_sort_index_default
    procedure, nopass :: int32_sort_index_default
    procedure, nopass :: int64_sort_index_default
    procedure, nopass :: sp_sort_index_default
    procedure, nopass :: dp_sort_index_default
    procedure, nopass :: xdp_sort_index_default
    procedure, nopass :: qp_sort_index_default
    procedure, nopass :: string_type_sort_index_default
    procedure, nopass :: char_sort_index_default
    procedure, nopass :: bitset_64_sort_index_default
    procedure, nopass :: bitset_large_sort_index_default
    procedure, nopass :: int8_sort_index_low
    procedure, nopass :: int16_sort_index_low
    procedure, nopass :: int32_sort_index_low
    procedure, nopass :: int64_sort_index_low
    procedure, nopass :: sp_sort_index_low
    procedure, nopass :: dp_sort_index_low
    procedure, nopass :: xdp_sort_index_low
    procedure, nopass :: qp_sort_index_low
    procedure, nopass :: string_type_sort_index_low
    procedure, nopass :: char_sort_index_low
    procedure, nopass :: bitset_64_sort_index_low
    procedure, nopass :: bitset_large_sort_index_low
    generic, public :: sort_index => int8_sort_index_default, &
                                      int16_sort_index_default, &
                                      int32_sort_index_default, &
                                      int64_sort_index_default, &
                                      sp_sort_index_default, &
                                      dp_sort_index_default, &
                                      xdp_sort_index_default, &
                                      qp_sort_index_default, &
                                      string_type_sort_index_default, &
                                      char_sort_index_default, &
                                      bitset_64_sort_index_default, &
                                      bitset_large_sort_index_default, &
                                      int8_sort_index_low, &
                                      int16_sort_index_low, &
                                      int32_sort_index_low, &
                                      int64_sort_index_low, &
                                      sp_sort_index_low, &
                                      dp_sort_index_low, &
                                      xdp_sort_index_low, &
                                      qp_sort_index_low, &
                                      string_type_sort_index_low, &
                                      char_sort_index_low, &
                                      bitset_64_sort_index_low, &
                                      bitset_large_sort_index_low

    ! final :: delete_ClassArrayMethod  !< Destructor
  end type ClassArrayMethod

contains

end module interface_stdlib_array
