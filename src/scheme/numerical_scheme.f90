!=============================================================================80
!
!> Gether all base method for temporal and spatial discretization schemes.
!
!=============================================================================80

module numerical_scheme

  use scheme_Runge_Kutta
  use scheme_reconstruct_simple
  use scheme_reconstruct_MUSCL
  use scheme_FDS_Roe
  use scheme_FVS_VanLeer
  use scheme_WENO
  use scheme_AUSM
  use scheme_gradient_Green
  use scheme_gradient_finite_diff
  use scheme_viscous_flux

  implicit none

!...Define scope
  private

  public :: TimeScheme, SpaceScheme

!...Declare temporal discretization schemes
  type :: ClassTemporalScheme
  contains
    procedure, public, nopass :: Runge_Kutta => Runge_Kutta_sub
  end type ClassTemporalScheme

!...Declare spatial discretization schemes
  type :: ClassSpatialScheme
  contains
    procedure, public, nopass :: reconstruct_simple => reconstruct_simple_sub
    procedure, public, nopass :: reconstruct_MUSCL  => reconstruct_MUSCL_sub
    procedure, public, nopass :: reconstruct_WENO   => reconstruct_WENO_sub
    procedure, public, nopass :: FDS_Roe     => FDS_Roe_sub
    procedure, public, nopass :: FVS_VanLeer => FVS_VanLeer_sub
    procedure, public, nopass :: FVS_AUSM    => FVS_AUSM_sub
    procedure, public, nopass :: gradient_Green => gradient_Green_sub
    procedure, public, nopass :: gradient_FD    => gradient_finite_diff_sub
    procedure, public, nopass :: viscous_flux   => viscous_flux_sub
  end type ClassSpatialScheme

!...Instance of class
  type(ClassTemporalScheme) :: TimeScheme
  type(ClassSpatialScheme)  :: SpaceScheme

end module numerical_scheme
