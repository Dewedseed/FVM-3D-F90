module defs_fluid_props

  use kind_parameter, only : dp

  implicit none

!...Define scope
  public

!...Define fluid properties
  type :: Thermal_Variable
    real(dp), public :: enthalpy = 0.0_dp
    real(dp), public :: entropy  = 0.0_dp
    real(dp), public :: energy   = 0.0_dp
  end type Thermal_Variable

  type :: Turbulent_Variable
    real(dp), public :: omega   = 0.0_dp
    real(dp), public :: epsilon = 0.0_dp
  end type Turbulent_Variable

  type :: Transport_Variable
    real(dp), public :: diff_coeff = 0.0_dp  !< Diffusion coefficient
    real(dp), public :: viscosity = 0.0_dp
    real(dp), public :: conductivity = 0.0_dp
  end type Transport_Variable

  type :: Reference_Variable
    real(dp), public :: length      = 1.0_dp
    real(dp), public :: density     = 0.0_dp
    real(dp), public :: velocity_x  = 0.0_dp
    real(dp), public :: velocity_y  = 0.0_dp
    real(dp), public :: velocity_z  = 0.0_dp
    real(dp), public :: pressure    = 0.0_dp
    real(dp), public :: temperature = 293.15_dp
  end type Reference_Variable

  ! type :: Dependent_Variable
  !   ! real(dp), public :: pressure
  !   real(dp), public :: temperature
  !   real(dp), public :: sound_speed
  !   real(dp), public :: gamma
  !   real(dp), public :: cp
  !   real(dp), public :: cv
  ! end type Dependent_Variable

  type :: Dependent_Variable
    ! real(dp), public :: p
    real(dp), public :: T = 0.0_dp !< static temperature
    real(dp), public :: c = 0.0_dp !< sound speed
    real(dp), public :: gamma = 0.0_dp !< specific heat ratio
    real(dp), public :: cp = 0.0_dp !< specific heat capacity at constant pressure
    real(dp), public :: cv = 0.0_dp !< specific heat capacity at constant volume
  end type Dependent_Variable

  ! type :: Primitive_Variable
  !   real(dp), public :: density
  !   real(dp), public :: velocity_x
  !   real(dp), public :: velocity_y
  !   real(dp), public :: velocity_z
  !   real(dp), public :: energy
  ! end type Primitive_Variable

  type :: Primitive_Variable
    real(dp), public :: rho = 0.0_dp
    real(dp), public :: u = 0.0_dp
    real(dp), public :: v = 0.0_dp
    real(dp), public :: w = 0.0_dp
    real(dp), public :: press = 0.0_dp  !< static pressure
  end type Primitive_Variable

  ! type :: Conserved_Variable
  !   real(dp), public :: density     !< density
  !   real(dp), public :: momentum_x  !< x-momentum (density*u)
  !   real(dp), public :: momentum_y  !< y-momentum (density*v)
  !   real(dp), public :: momentum_z  !< z-momentum (density*w)
  !   real(dp), public :: energy      !< total energy per unit volume (density*E)
  ! end type Conserved_Variable

  type :: Conserved_Variable
    real(dp), public :: rho  = 0.0_dp   !< density
    real(dp), public :: momx = 0.0_dp  !< x-momentum (density*u)
    real(dp), public :: momy = 0.0_dp  !< y-momentum (density*v)
    real(dp), public :: momz = 0.0_dp  !< z-momentum (density*w)
    real(dp), public :: ener = 0.0_dp  !< total energy per unit volume (density*E)
  end type Conserved_Variable

end module defs_fluid_props
