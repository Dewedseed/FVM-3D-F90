!=============================================================================80
!
!> String method.
!
! https://stdlib.fortran-lang.org/page/specs/stdlib_strings.html
! https://stdlib.fortran-lang.org/page/specs/stdlib_str2num.html
! https://stdlib.fortran-lang.org/page/specs/stdlib_ascii.html
!
!=============================================================================80

module interface_stdlib_string

  use iso_fortran_env, only : int8, int16, int32, int64
  use kind_parameter,  only : i1, i2, i4, i8, sp, dp, qp

  use stdlib_str2num, only :  to_int8, to_int16, to_int32, to_int64, &
                              to_sp, to_dp, to_xdp, to_qp

  use stdlib_str2num, only :  to_int8_from_stream, to_int16_from_stream, &
                              to_int32_from_stream, to_int64_from_stream, &
                              to_sp_from_stream, to_dp_from_stream, &
                              to_xdp_from_stream, to_qp_from_stream

  use stdlib_strings, only :  strip_char, strip_string

  use stdlib_strings, only :  chomp_string, &
                              chomp_char, &
                              chomp_set_string_char, &
                              chomp_set_char_char, &
                              chomp_substring_string_string, &
                              chomp_substring_char_string, &
                              chomp_substring_string_char, &
                              chomp_substring_char_char

  use stdlib_strings, only :  starts_with_string_string, &
                              starts_with_string_char, &
                              starts_with_char_string, &
                              starts_with_char_char

  use stdlib_strings, only :  ends_with_string_string, &
                              ends_with_string_char, &
                              ends_with_char_string, &
                              ends_with_char_char

  use stdlib_strings, only :  slice_string, slice_char

  use stdlib_strings, only :  find_string_string, &
                              find_string_char, &
                              find_char_string, &
                              find_char_char

  use stdlib_strings, only :  replace_all_string_string_string, &
                              replace_all_string_string_char, &
                              replace_all_string_char_string, &
                              replace_all_char_string_string, &
                              replace_all_char_string_char, &
                              replace_all_string_char_char, &
                              replace_all_char_char_string, &
                              replace_all_char_char_char

  use stdlib_strings, only :  padl_string_default, &
                              padl_string_pad_with, &
                              padl_char_default, &
                              padl_char_pad_with

  use stdlib_strings, only :  padr_string_default, &
                              padr_string_pad_with, &
                              padr_char_default, &
                              padr_char_pad_with

  use stdlib_strings, only :  count_string_string, &
                              count_string_char, &
                              count_char_string, &
                              count_char_char

  use stdlib_strings, only :  zfill_string, &
                              zfill_char

  use stdlib_strings, only :  to_string_r_sp, &
                              to_string_r_dp, &
                              to_string_r_xdp, &
                              to_string_r_qp, &
                              to_string_c_sp, &
                              to_string_c_dp, &
                              to_string_c_xdp, &
                              to_string_c_qp, &
                              to_string_1_i_int8, &
                              to_string_2_i_int8, &
                              to_string_1_i_int16, &
                              to_string_2_i_int16, &
                              to_string_1_i_int32, &
                              to_string_2_i_int32, &
                              to_string_1_i_int64, &
                              to_string_2_i_int64, &
                              to_string_1_l_lk, &
                              to_string_2_l_lk, &
                              to_string_1_l_c_bool, &
                              to_string_2_l_c_bool

  use stdlib_ascii, only: to_lower, to_upper, to_title, to_sentence, reverse

  implicit none

!...Define scope
  private

  public  :: ClassStringMethod

!...Declare local variables
  type :: ClassStringMethod
  contains
  !...strip
    !> example: strip("   hello   ") => "hello"
    procedure, private, nopass :: strip_char => strip_char
    procedure, private, nopass :: strip_string => strip_string
    generic, public :: strip => strip_char, strip_string

  !...chomp
    !> example: chomp("   hello   ") => "   hello"
    !!
    !!          chomp("hello", ["l", "o"]) => "he"
    !!
    !!          chomp("hello", substring="lo") => "hel"
    !!
    procedure, nopass, private :: chomp_string => chomp_string
    procedure, nopass, private :: chomp_char => chomp_char
    procedure, nopass, private :: chomp_set_string_char => chomp_set_string_char
    procedure, nopass, private :: chomp_set_char_char => chomp_set_char_char
    procedure, nopass, private :: chomp_substring_string_string => chomp_substring_string_string
    procedure, nopass, private :: chomp_substring_char_string => chomp_substring_char_string
    procedure, nopass, private :: chomp_substring_string_char => chomp_substring_string_char
    procedure, nopass, private :: chomp_substring_char_char => chomp_substring_char_char
    generic, public :: chomp => chomp_string, &
                                  chomp_char, &
                                  chomp_set_string_char, &
                                  chomp_set_char_char, &
                                  chomp_substring_string_string, &
                                  chomp_substring_char_string, &
                                  chomp_substring_string_char, &
                                  chomp_substring_char_char

  !...starts_with
    !> example: starts_with("hello", "he") => .true.
    procedure, nopass, private :: starts_with_string_string => starts_with_string_string
    procedure, nopass, private :: starts_with_string_char => starts_with_string_char
    procedure, nopass, private :: starts_with_char_string => starts_with_char_string
    procedure, nopass, private :: starts_with_char_char => starts_with_char_char
    generic, public :: starts_with => starts_with_string_string, &
                                        starts_with_string_char, &
                                        starts_with_char_string, &
                                        starts_with_char_char
  !...ends_with
    !> example: ends_with("hello", "lo") => .true.
    procedure, public, nopass :: ends_with_string_string => ends_with_string_string
    procedure, public, nopass :: ends_with_string_char => ends_with_string_char
    procedure, public, nopass :: ends_with_char_string => ends_with_char_string
    procedure, public, nopass :: ends_with_char_char => ends_with_char_char
    generic, public :: ends_with => ends_with_string_string, &
                                      ends_with_string_char, &
                                      ends_with_char_string, &
                                      ends_with_char_char

  !...slice
    !> example: slice("abcdefghij", 2, 6, 2) => "bdf"
    procedure, nopass, private :: slice_string => slice_string
    procedure, nopass, private :: slice_char => slice_char
    generic, public :: slice => slice_string, &
                                  slice_char

  !...find
    !> example: find(string, ["a", "c"], [3, 2]) => [27, 20]
    procedure, nopass, private :: find_string_string => find_string_string
    procedure, nopass, private :: find_string_char => find_string_char
    procedure, nopass, private :: find_char_string => find_char_string
    procedure, nopass, private :: find_char_char => find_char_char
    generic, public :: find => find_string_string, &
                                  find_string_char, &
                                  find_char_string, &
                                  find_char_char

  !...replace_all
    !> example: replace_all("hurdles here", "hurdles", "technology") => "technology here"
    procedure, nopass, private :: replace_all_string_string_string => replace_all_string_string_string
    procedure, nopass, private :: replace_all_string_string_char => replace_all_string_string_char
    procedure, nopass, private :: replace_all_string_char_string => replace_all_string_char_string
    procedure, nopass, private :: replace_all_char_string_string => replace_all_char_string_string
    procedure, nopass, private :: replace_all_char_string_char => replace_all_char_string_char
    procedure, nopass, private :: replace_all_string_char_char => replace_all_string_char_char
    procedure, nopass, private :: replace_all_char_char_string => replace_all_char_char_string
    procedure, nopass, private :: replace_all_char_char_char => replace_all_char_char_char
    generic, public :: replace_all => replace_all_string_string_string, &
                                        replace_all_string_string_char, &
                                        replace_all_string_char_string, &
                                        replace_all_char_string_string, &
                                        replace_all_char_string_char, &
                                        replace_all_string_char_char, &
                                        replace_all_char_char_string, &
                                        replace_all_char_char_char

  !...padl and padr
    !> example: padl("hello", 10)    => "     hello"
    !!
    !!          padl("hello", 10, $) => "$$$$$hello"
    procedure, nopass, private :: padl_string_default => padl_string_default
    procedure, nopass, private :: padl_string_pad_with => padl_string_pad_with
    procedure, nopass, private :: padl_char_default => padl_char_default
    procedure, nopass, private :: padl_char_pad_with => padl_char_pad_with
    generic, public :: padl => padl_string_default, &
                                 padl_string_pad_with, &
                                 padl_char_default, &
                                 padl_char_pad_with

    !! example: padr("hello", 10)    => "hello      "
    !!
    !!          padr("hello", 10, $) => "hello$$$$$"
    procedure, nopass, private :: padr_string_default => padr_string_default
    procedure, nopass, private :: padr_string_pad_with => padr_string_pad_with
    procedure, nopass, private :: padr_char_default => padr_char_default
    procedure, nopass, private :: padr_char_pad_with => padr_char_pad_with
    generic, public :: padr => padr_string_default, &
                                 padr_string_pad_with, &
                                 padr_char_default, &
                                 padr_char_pad_with

  !...count
    !! example: count("hello", "l") => 3
    procedure, nopass, private :: count_string_string => count_string_string
    procedure, nopass, private :: count_string_char => count_string_char
    procedure, nopass, private :: count_char_string => count_char_string
    procedure, nopass, private :: count_char_char => count_char_char
    generic, public :: count => count_string_string, &
                                  count_string_char, &
                                  count_char_string, &
                                  count_char_char

  !...zfill
    !! example: zfill("hello", 10) => "00000hello"
    procedure, nopass, private :: zfill_string => zfill_string
    procedure, nopass, private :: zfill_char => zfill_char
    generic, public :: zfill => zfill_string, &
                                  zfill_char


  !...other_to_string
    !> to_string (value [, format])
    !!
    !! example: from((1, 1))              => "(1.00000000,1.00000000)"
    !!
    !!          from((1, 1), '(F6.2)')    => "(  1.00,  1.00)"
    !!
    !!          from(42, '(I4)')          => "  42"
    !!
    !!          from(.true.)              => "T"
    procedure, nopass, private :: to_string_r_sp => to_string_r_sp
    procedure, nopass, private :: to_string_r_dp => to_string_r_dp
    procedure, nopass, private :: to_string_r_xdp => to_string_r_xdp
    procedure, nopass, private :: to_string_r_qp => to_string_r_qp
    procedure, nopass, private :: to_string_c_sp => to_string_c_sp
    procedure, nopass, private :: to_string_c_dp => to_string_c_dp
    procedure, nopass, private :: to_string_c_xdp => to_string_c_xdp
    procedure, nopass, private :: to_string_c_qp => to_string_c_qp
    procedure, nopass, private :: to_string_1_i_int8 => to_string_1_i_int8
    procedure, nopass, private :: to_string_2_i_int8 => to_string_2_i_int8
    procedure, nopass, private :: to_string_1_i_int16 => to_string_1_i_int16
    procedure, nopass, private :: to_string_2_i_int16 => to_string_2_i_int16
    procedure, nopass, private :: to_string_1_i_int32 => to_string_1_i_int32
    procedure, nopass, private :: to_string_2_i_int32 => to_string_2_i_int32
    procedure, nopass, private :: to_string_1_i_int64 => to_string_1_i_int64
    procedure, nopass, private :: to_string_2_i_int64 => to_string_2_i_int64
    procedure, nopass, private :: to_string_1_l_lk => to_string_1_l_lk
    procedure, nopass, private :: to_string_2_l_lk => to_string_2_l_lk
    procedure, nopass, private :: to_string_1_l_c_bool => to_string_1_l_c_bool
    procedure, nopass, private :: to_string_2_l_c_bool => to_string_2_l_c_bool
    generic, public :: from => to_string_r_sp, &
                               to_string_r_dp, &
                               to_string_r_xdp, &
                               to_string_r_qp, &
                               to_string_c_sp, &
                               to_string_c_dp, &
                               to_string_c_xdp, &
                               to_string_c_qp, &
                               to_string_1_i_int8, &
                               to_string_2_i_int8, &
                               to_string_1_i_int16, &
                               to_string_2_i_int16, &
                               to_string_1_i_int32, &
                               to_string_2_i_int32, &
                               to_string_1_i_int64, &
                               to_string_2_i_int64, &
                               to_string_1_l_lk, &
                               to_string_2_l_lk, &
                               to_string_1_l_c_bool, &
                               to_string_2_l_c_bool

  !...to_num
    !! example: to_num("42", int)  => 42
    !!          to_num("42", real) => 42.0
    procedure, nopass, private :: to_int8 => to_int8
    procedure, nopass, private :: to_int16 => to_int16
    procedure, nopass, private :: to_int32 => to_int32
    procedure, nopass, private :: to_int64 => to_int64
    procedure, nopass, private :: to_sp => to_sp
    procedure, nopass, private :: to_dp => to_dp
    procedure, nopass, private :: to_xdp => to_xdp
    procedure, nopass, private :: to_qp => to_qp
    generic,   public :: to_num_std => to_int8, &
                                       to_int16, &
                                       to_int32, &
                                       to_int64, &
                                       to_sp, &
                                       to_dp, &
                                       to_xdp, &
                                       to_qp

  !...to_num_from_stream
    !! example: r(i) = to_num_from_stream( cptr , r(i) )
    !!                    the pointer cptr is shifted within the function
    procedure, nopass, private :: to_int8_from_stream => to_int8_from_stream
    procedure, nopass, private :: to_int16_from_stream => to_int16_from_stream
    procedure, nopass, private :: to_int32_from_stream => to_int32_from_stream
    procedure, nopass, private :: to_int64_from_stream => to_int64_from_stream
    procedure, nopass, private :: to_sp_from_stream => to_sp_from_stream
    procedure, nopass, private :: to_dp_from_stream => to_dp_from_stream
    procedure, nopass, private :: to_xdp_from_stream => to_xdp_from_stream
    procedure, nopass, private :: to_qp_from_stream => to_qp_from_stream
    generic, public :: to_num_from_stream => to_int8_from_stream, &
                                              to_int16_from_stream, &
                                              to_int32_from_stream, &
                                              to_int64_from_stream, &
                                              to_sp_from_stream, &
                                              to_dp_from_stream, &
                                              to_xdp_from_stream, &
                                              to_qp_from_stream

  !...to number (array)
    procedure, nopass, private :: to_int8__ => to_int8__
    procedure, nopass, private :: to_int16__ => to_int16__
    procedure, nopass, private :: to_int32__ => to_int32__
    procedure, nopass, private :: to_int64__ => to_int64__
    procedure, nopass, private :: to_sp__ => to_sp__
    procedure, nopass, private :: to_dp__ => to_dp__
    procedure, nopass, private :: to_qp__ => to_qp__
    procedure, nopass, private :: to_int8_array__ => to_int8_array__
    procedure, nopass, private :: to_int16_array__ => to_int16_array__
    procedure, nopass, private :: to_int32_array__ => to_int32_array__
    procedure, nopass, private :: to_int64_array__ => to_int64_array__
    procedure, nopass, private :: to_sp_array__ => to_sp_array__
    procedure, nopass, private :: to_dp_array__ => to_dp_array__
    procedure, nopass, private :: to_qp_array__ => to_qp_array__
    generic, public :: to_num => to_int8__, &
                                 to_int16__, &
                                 to_int32__, &
                                 to_int64__, &
                                 to_sp__, &
                                 to_dp__, &
                                 to_qp__, &
                                 to_int8_array__, &
                                 to_int16_array__, &
                                 to_int32_array__, &
                                 to_int64_array__, &
                                 to_sp_array__, &
                                 to_dp_array__, &
                                 to_qp_array__

  !...to logical
    procedure, public, nopass :: to_logical => to_logical_from_string

  !...to_lower and to_upper
    !! example: to_lower("Hello") => "hello"
    procedure, public, nopass :: to_lower => to_lower
    !! example: to_upper("Hello") => "HELLO"
    procedure, public, nopass :: to_upper => to_upper
    !! example: to_title("hello") => "Hello"
    procedure, public, nopass :: to_title => to_title
    !! example: to_sentence("hello") => "Hello"
    procedure, public, nopass :: to_sentence => to_sentence

  !...reverse
    !! example: reverse("hello") => "olleh"
    procedure, public, nopass :: reverse => reverse
  !...Destructor
    ! final :: delete_ClassStringMethod  !< Destructor

  end type ClassStringMethod

contains

!============================= String To Logical =============================80
!
! Convert string to logical
!
!    to_logical_from_string
!
!=============================================================================80

  !> Convert string to logical
  logical function to_logical_from_string(string, stat) result(res)
    character(len=*), intent(in)  :: string
    integer,          intent(out) :: stat
  continue
    stat = 0
    select case (trim(adjustl(string)))
      case ('.true.', 'True', 'T', 'TRUE')
        res = .true.
      case ('.false.', 'False', 'F', 'FALSE')
        res = .false.
      case ('', 'MISS')
        res = .false.
        stat = -1
      case default
        res = .false.
        stat = 1
    end select
  end function to_logical_from_string

!============================= String To Number ==============================80
!
! Convert string to number by <read> function.
!
!    to_int8__      i1 => int8
!
!=============================================================================80

  !> Convert string to number (i1 => int8)
  function to_int8__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i1),      intent(in)  :: mold
    integer,          intent(out) :: stat
    integer(i1)                   :: val
  continue
    val = 0_i1
    read(string, *, iostat=stat) val
  end function to_int8__
  !> Convert string to number array (i1 => int8)
  function to_int8_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i1),      intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    integer(i1)                   :: val(size)
  continue
    val = 0_i1
    read(string, *, iostat=stat) val
  end function to_int8_array__

  !> Convert string to number (i2 => int16)
  function to_int16__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i2),      intent(in)  :: mold
    integer,          intent(out) :: stat
    integer(i2)                   :: val
  continue
    val = 0_i2
    read(string, *, iostat=stat) val
  end function to_int16__
  !> Convert string to number array (i2 => int16)
  function to_int16_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i2),      intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    integer(i2)                   :: val(size)
  continue
    val = 0_i2
    read(string, *, iostat=stat) val
  end function to_int16_array__

  !> Convert string to number (i4 => int32)
  function to_int32__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i4),      intent(in)  :: mold
    integer,          intent(out) :: stat
    integer(i4)                   :: val
  continue
    val = 0_i4
    read(string, *, iostat=stat) val
  end function to_int32__
  !> Convert string to number array (i4 => int32)
  function to_int32_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i4),      intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    integer(i4)                   :: val(size)
  continue
    val = 0_i4
    read(string, *, iostat=stat) val
  end function to_int32_array__

  !> Convert string to number (i8 => int64)
  function to_int64__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i8),      intent(in)  :: mold
    integer,          intent(out) :: stat
    integer(i8)                   :: val
  continue
    val = 0_i8
    read(string, *, iostat=stat) val
  end function to_int64__

  !> Convert string to number array (i8 => int64)
  function to_int64_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i8),      intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    integer(i8)                   :: val(size)
  continue
    val = 0_i8
    read(string, *, iostat=stat) val
  end function to_int64_array__

  !> Convert string to number (sp => single precision)
  function to_sp__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    real(sp),         intent(in)  :: mold
    integer,          intent(out) :: stat
    real(sp)                      :: val
  continue
    val = 0.0_sp
    read(string, *, iostat=stat) val
  end function to_sp__

  !> Convert string to number array (sp => single precision)
  function to_sp_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    real(sp),         intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    real(sp)                      :: val(size)
  continue
    val = 0.0_sp
    read(string, *, iostat=stat) val
  end function to_sp_array__

  !> Convert string to number (dp => double precision)
  function to_dp__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    real(dp),         intent(in)  :: mold
    integer,          intent(out) :: stat
    real(dp)                      :: val
  continue
    val = 0.0_dp
    read(string, *, iostat=stat) val
  end function to_dp__

  !> Convert string to number array (dp => double precision)
  function to_dp_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    real(dp),         intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    real(dp)                      :: val(size)
  continue
    val = 0.0_dp
    read(string, *, iostat=stat) val
  end function to_dp_array__

  !> Convert string to number (qp => quadruple precision)
  function to_qp__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    real(qp),         intent(in)  :: mold
    integer,          intent(out) :: stat
    real(qp)                      :: val
  continue
    val = 0.0_qp
    read(string, *, iostat=stat) val
  end function to_qp__

  !> Convert string to number array (qp => quadruple precision)
  function to_qp_array__(string, mold, size, stat) result(val)
    character(len=*), intent(in)  :: string
    real(qp),         intent(in)  :: mold
    integer(i4),      intent(in)  :: size
    integer,          intent(out) :: stat
    real(qp)                      :: val(size)
  continue
    val = 0.0_qp
    read(string, *, iostat=stat) val
  end function to_qp_array__

!======================= String To Number By StdLib ==========================80
!
! Convert string to number by stdlib.
!
!    to_int8_std__      i1 => int8
!    to_int16_std__     i2 => int16
!    to_int32_std__     i4 => int32
!    to_int64_std__     i8 => int64
!    to_sp_std__        sp => single precision
!    to_dp_std__        dp => double precision
!    to_xdp_std__       xdp => extended double precision
!    to_qp_std__        qp => quadruple precision
!
!  to_num_from_stream:   stat = 23
!
!=============================================================================80

  !> Convert string to number (i1 => int8)
  function to_int8_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i1),      intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    integer(i1)                   :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_int8_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_int8_std__

  !> Convert string to number (i2 => int16)
  function to_int16_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i2),      intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    integer(i2)                   :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_int16_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_int16_std__

  !> Convert string to number (i4 => int32)
  function to_int32_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i4),      intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    integer(i4)                   :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_int32_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_int32_std__

  !> Convert string to number (i8 => int64)
  function to_int64_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    integer(i8),      intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    integer(i8)                   :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_int64_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_int64_std__

  !> Convert string to number (sp => single precision)
  function to_sp_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    real(sp),         intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    real(sp)                      :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_sp_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_sp_std__

  !> Convert string to number (dp => double precision)
  function to_dp_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    real(dp),         intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    real(dp)                      :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_dp_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_dp_std__

  !> Convert string to number (qp => quadruple precision)
  function to_qp_std__(string, mold, stat) result(val)
    character(len=*), intent(in)  :: string
    real(qp),         intent(in)  :: mold
    integer(i1),      intent(out) :: stat
    real(qp)                      :: val
  !...Declare local variables
    character(len=:), pointer :: str
  continue
    str = trim(string)
    val = to_qp_from_stream(str, mold, stat=stat)
    deallocate(str)
  end function to_qp_std__

end module interface_stdlib_string
