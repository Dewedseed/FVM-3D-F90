!=============================================================================80
!
!> Compute viscous fluxes (viscous stress tensor and heat flux)
!
!=============================================================================80

module scheme_viscous_flux

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: viscous_flux_sub

contains

!================================ SUB_NAME ===================================80
!
!> Compute viscous fluxes
!
! TODO: use array-mutiplication
!
!=============================================================================80

  subroutine viscous_flux_sub(nface, neq, Pr, mu, norm, p, grad, gradT, flux)
    use defs_fluid_props, only : Primitive_Variable
  !...Declare input/output variables
    integer(i8),              intent(in)    :: nface !< Number of faces
    integer,                  intent(in)    :: neq   !< Number of equations
    real(dp),                 intent(in)    :: Pr    !< Prandtl number
    real(dp),                 intent(in)    :: mu(nface)      !< dynamic viscosity
    real(dp),                 intent(in)    :: norm (nface,3) !< Normal vector
    type(Primitive_Variable), intent(in)    :: p(nface)       !< Primitive variable
    type(Primitive_Variable), intent(inout) :: grad (nface,3) !< Gradient of primitive
    real(dp),                 intent(inout) :: gradT(nface,3) !< Gradient of temperature
    real(dp),                 intent(out)   :: flux (nface,neq) !< Viscous fluxes
  !...Declare local variable
    real(dp), dimension(nface) :: lambda !< second viscosity coefficient
    ! viscous stress tensor
    real(dp), dimension(nface) :: tau_xx, tau_xy, tau_xz
    real(dp), dimension(nface) :: tau_yx, tau_yy, tau_yz
    real(dp), dimension(nface) :: tau_zx, tau_zy, tau_zz
    ! gradient
    real(dp), dimension(nface) :: dudx, dudy, dudz
    real(dp), dimension(nface) :: dvdx, dvdy, dvdz
    real(dp), dimension(nface) :: dwdx, dwdy, dwdz
    real(dp), dimension(nface) :: dhdx, dhdy, dhdz !< inclue cp
    ! energy
    real(dp), dimension(nface) :: Theta_x, Theta_y, Theta_z
  continue
  !...Get gradient
    dudx = grad(:,1)%u
    dudy = grad(:,2)%u
    dudz = grad(:,3)%u

    dvdx = grad(:,1)%v
    dvdy = grad(:,2)%v
    dvdz = grad(:,3)%v

    dwdx = grad(:,1)%w
    dwdy = grad(:,2)%w
    dwdz = grad(:,3)%w

    dhdx = gradT(:,1)
    dhdy = gradT(:,2)
    dhdz = gradT(:,3)
  !...Compute viscous stress tensor
    lambda = - 2.0_dp / 3.0_dp * mu

    tau_xx = lambda * (dudx + dvdy + dwdz) + 2.0_dp * mu * dudx
    tau_yy = lambda * (dudx + dvdy + dwdz) + 2.0_dp * mu * dvdy
    tau_zz = lambda * (dudx + dvdy + dwdz) + 2.0_dp * mu * dwdz

    tau_xy = mu * (dudy + dvdx)
    tau_yx = tau_xy

    tau_xz = mu * (dudz + dwdx)
    tau_zx = tau_xz

    tau_yz = mu * (dvdz + dwdy)
    tau_zy = tau_yz
  !...Compute energy viscous
    Theta_x = p%u * tau_xx + p%v * tau_xy + p%w * tau_xz + mu / Pr * dhdx
    Theta_y = p%u * tau_yx + p%v * tau_yy + p%w * tau_yz + mu / Pr * dhdy
    Theta_z = p%u * tau_zx + p%v * tau_zy + p%w * tau_zz + mu / Pr * dhdz
  !...Compute viscous flux
    flux(:, 1) = 0.0_dp
    flux(:, 2) = norm(:,1) * tau_xx  + norm(:,2) * tau_xy  + norm(:,3) * tau_xz
    flux(:, 3) = norm(:,1) * tau_yx  + norm(:,2) * tau_yy  + norm(:,3) * tau_yz
    flux(:, 4) = norm(:,1) * tau_zx  + norm(:,2) * tau_zy  + norm(:,3) * tau_zz
    flux(:, 5) = norm(:,1) * Theta_x + norm(:,2) * Theta_y + norm(:,3) * Theta_z
  end subroutine viscous_flux_sub

end module scheme_viscous_flux
