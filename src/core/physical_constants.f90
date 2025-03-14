!=============================================================================80
!
!> Denfine physical parameters.
!
! REF: https://doc.comsol.com/5.5/doc/com.comsol.help.comsol/comsol_ref_definitions.12.025.html
!
!=============================================================================80

module physical_constants

  use kind_parameter, only : dp

  implicit none

!...Define scope
  public

!...Declare global variables
  real(dp), parameter :: g_const = 9.80665
                          !< [m/s^2]
                          !! Acceleration of gravity
  real(dp), parameter :: N_A_const = 6.02214076e23
                          !< [1/mol]
                          !! Avogadro constant
  real(dp), parameter :: k_B_const = 1.380649e-23
                          !< [J/K]
                          !! Boltzmann constant
  real(dp), parameter :: Z0_const = 376.730313461
                          !< [ohm] (mu0*c)
                          !! Characteristic impedance of vacuum (impedance of free space)
  real(dp), parameter :: me_const = 9.10938356e-31
                          !< [kg]
                          !! Electron mass
  real(dp), parameter :: e_const = 1.602176634e-19
                          !< [C]
                          !! Elementary charge
  real(dp), parameter ::  F_const = 96485.33289
                          !< [C/mol]
                          !! Faraday constant
  real(dp), parameter ::  alpha_const = 7.2973525664e-3
                          !< Fine-structure constant
  real(dp), parameter ::  Grav_const = 6.67408e-11
                          !< [m^3/(kg*s^2)]
                          !! Gravitational constant
  real(dp), parameter :: V_m_const = 22.413962e-3
                          !< [m^3/mol]
                          !! Molar volume of ideal gas (at 273.15 K and 1 atm)
  real(dp), parameter :: mn_const = 1.674927471e-27
                          !< [kg]
                          !! Neutron mass
  real(dp), parameter ::  c_const = 299792458
                          !< [m/s]
                          !! Speed of light in vacuum
  real(dp), parameter ::  h_const = 6.62607015e-34
                          !< [J*s]
                          !! Planck’s constant
  real(dp), parameter ::  hbar_const = 1.05457180e-34
                          !< [J*s]
                          !! Planck’s constant over 2 pi
  real(dp), parameter ::  mp_const = 1.672621898e-27
                          !< [kg]
                          !! Proton mass
  real(dp), parameter ::  sigma_const = 5.670367e-8
                          !< [W/(m^2*K^4)]
                          !! Stefan-Boltzmann constant
  real(dp), parameter ::  R_const = 8.3144598
                          !< [J/(mol*K)]
                          !! Universal gas constant (molar gas constant)
  real(dp), parameter ::  b_const = 2.8977729e-3
                          !< [m*K]
                          !! Wien displacement law constant
  real(dp), parameter :: mu0_const = 2*alpha_const*h_const/c_const/e_const/e_const
                          !< (H/m)
                          !! Permeability of vacuum (magnetic constant)
  real(dp), parameter :: epsilon0_const = 1/mu0_const/c_const/c_const
                          !< (F/m)
                          !! Permittivity of vacuum (electric constant)

end module physical_constants
