!=============================================================================80
!
!> Set solver basic information.
!
!  Record of Revisions
!      Date       Version    Programmer     Description of change
!      ====       =======    ==========     =====================
!   2024/08/19     0.0.0     Dewedseed      Original Code
!   2024/11/26     0.0.1     Dewedseed      Change Class Scope for Safety
!   2024/12/12     0.0.2     Dewedseed      Add memory pool; Change Data Structure
!   2025/03/15     0.0.3     Dewedseed      Change design pattern
!
!=============================================================================80

module class_solver_info

  use kind_parameter, only : len_string

  implicit none

!...Define scope
  private

  public :: ClassSolverInfo

!...Declare local variables
  type :: ClassSolverInfo
    character(len_string), public :: name     = "FVM-3D-CFD"
    character(len_string), public :: version  = "0.0.3"
    character(len_string), public :: compiler = "gfortran"
  end type ClassSolverInfo

end module class_solver_info
