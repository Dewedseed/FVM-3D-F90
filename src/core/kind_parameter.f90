!=============================================================================80
!
!> Numerical storage size parameters for real and integer values
!
! Ref: https://fortran-lang.org/en/learn/best_practices/style_guide
!
!=============================================================================80

module kind_parameter

  implicit none

  public

!...Define precision of real numbers
  integer, parameter :: sp = selected_real_kind(6, 37)
                      !< Single precision real numbers, 6 digits;
                      !< range 10⁻³⁷ to 10³⁷-1; 32 bits
  integer, parameter :: dp = selected_real_kind(15, 307)
                      !< Double precision real numbers, 15 digits
                      !< range 10⁻³⁰⁷ to 10³⁰⁷-1; 64 bits
  integer, parameter :: qp = selected_real_kind(33, 4931)
                      !< Quadruple precision real numbers, 33 digits;
                      !< range 10⁻⁴⁹³¹ to 10⁴⁹³¹-1; 128 bits

!...Define precision of intergers
  integer, parameter :: i1 = selected_int_kind(2)
                      !< Char length for integers,
                      !< range -2⁷ to 2⁷-1; 8 bits
  integer, parameter :: i2 = selected_int_kind(4)
                      !< Short length for integers,
                      !< range -2¹⁵ to 2¹⁵-1; 16 bits
  integer, parameter :: i4 = selected_int_kind(9)
                      !< Length of default integers,
                      !< range -2³¹ to 2³¹-1; 32 bits
  integer, parameter :: i8 = selected_int_kind(18)
                      !< Long length for integers,
                      !< range -2⁶³ to 2⁶³-1; 64 bits

!...Define length of string
  integer, parameter :: len_short  = 64
                      !< Length of short string
  integer, parameter :: len_string = 256
                      !< Length of string
  integer, parameter :: len_long   = 512
                      !< Length of long string

!...Define paramter for type
  integer(i4),  parameter :: int    = 0_i4
  integer(i8),  parameter :: int8   = 0_i8
  real(sp),     parameter :: float  = 0.0_sp
  real(dp),     parameter :: double = 0.0_dp
  character(6), parameter :: str    = "string"
  logical,      parameter :: logic  = .true.

!...Define infinite value
  integer(i1),  parameter :: i1_inf = huge(0_i1)
  integer(i2),  parameter :: i2_inf = huge(0_i2)
  integer(i4),  parameter :: i4_inf = huge(0_i4)
  integer(i8),  parameter :: i8_inf = huge(0_i8)

  real(dp),     parameter :: dp_inf = huge(0.0_dp)
  real(sp),     parameter :: sp_inf = huge(0.0_sp)
  real(qp),     parameter :: qp_inf = huge(0.0_qp)

!...Define epsilon value
  real(dp), parameter :: dp_eps = epsilon(1.0_dp)
  real(sp), parameter :: sp_eps = epsilon(1.0_sp)
  real(qp), parameter :: qp_eps = epsilon(1.0_qp)

end module kind_parameter
