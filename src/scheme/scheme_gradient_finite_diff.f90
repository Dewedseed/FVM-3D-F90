!=============================================================================80
!
! Compute primitive variable gradient using finite difference for viscous fluxes.
!
! TODO: this is for simple case (geometry)
!
!=============================================================================80

module scheme_gradient_finite_diff

  use kind_parameter, only : dp, i8
  use global_devlog,  only : devlog, LogLevel

  implicit none
  private

  public :: gradient_finite_diff_sub

contains

!=========================== GRADIENT_FINITE_DIFF_SUB ========================80
!
!> Compute primitive variable gradient using finite difference. (Curviliear Coordinate)
!
!         * --------- * --------- * --------- *
!         |           |           |           |
!         |           x           x           |
!         |           |           |           |
!         * --------- * --- o --- * --------- *
!         |           |           |           |
!         |           x           x           |
!         |           |           |           |
!         * --------- * --------- * --------- *
!
!      x direction = i direction = xi direction
!       geometry       mesh        curvilinear
!
!  U -- Flow variable
!
!     U(i+1/2,j,k)_I = (U(i+1,j,k) - U(i,j,k)) / dI
!
!     U(i+1/2,j,k)_J = 1/4 * (U(i,j-1/2,k)_J + U(i,j+1/2,k)_J +
!                       U(i+1,j-1/2,k)_J + U(i+1,j+1/2,k)_J) / dJ
!       where  U(i,j+1/2,k)_J = (U(i,j+1,k) - U(i,j,k)) / dJ
!  => U(i,j+1/2,k)_J = 1/2 * (U(i,j+1,k) - U(i,j-1,k)) / dJ +
!                      1/2 * (U(i+1,j+1,k) - U(i+1,j-1,k)) / dJ
!
!     U(i+1/2,j,k)_K = 1/4 * (U(i,j,k-1/2)_K + U(i,j,k+1/2)_K +
!                       U(i+1,j,k-1/2)_K + U(i+1,j,k+1/2)_K) / dK
!       where  U(i,j,k+1/2)_K = (U(i,j,k+1) - U(i,j,k)) / dK
!  => U(i,j,k+1/2)_K = 1/2 * (U(i,j,k+1) - U(i,j,k-1)) / dK +
!                      1/2 * (U(i+1,j,k+1) - U(i+1,j,k-1)) / dK
!
!=============================================================================80

  subroutine gradient_finite_diff_sub(nface, dghost, dr, dir, pg, grad)
  !...Declare input/output variables
    integer(i8), intent(in)    :: nface(3)  !< number of face (i,j,k)
    integer(i8), intent(in)    :: dghost(3) !< ghost cell size (ig = i + dghost(1), etc.)
    real(dp),    intent(in)    :: dr(3)     !< cell size (dI, dJ, dK)
    integer,     intent(in)    :: dir       !< direction (1: x, 2: y, 3: z)
    real(dp),    intent(in)    :: pg(:,:,:) !< flow variable on ghost mesh
    real(dp),    intent(inout) :: grad(:,:,:,:)
                            !< flow variable gradient:
                            !<   du/di(i+1/2,j,k) = grad(1:nface, j, k, 1),
                            !<   du/di(i+1/2,j,k) = grad(1:nface, j, k, 2),
                            !<   du/dj(i,j+1/2,k) = grad(i, 1:nface, k, 2)
                            !<   du/dk(i,j,k+1/2) = grad(i, j, 1:nface, 3)
  !...Declare local variable
    !
    !    :       :       |       |       |       :       :
    !    + ..... + ..x.. * --x-- * --x-- * ..x.. + ..... +
    !    :       :       |       |       |       :       :
    !    :       x       x       x       x       x       :
    !    :       :       |       |       |       :       :
    !    + ..... + ..x.. * --x-- * --x-- * ..x.. + ..... +
    !    :       :       :       :       :       :       :
    !    :       x       x       x       x       x       :
    !    :       :       :       :       :       :       :
    !    + ..... + ..x.. * --x-- * --x-- * ..x.. + ..... +
    !
    real(dp) :: gradii(nface(1)+1, nface(2)+1, nface(3)+1)
    real(dp) :: gradjj(nface(1)+1, nface(2)+1, nface(3)+1)
    real(dp) :: gradkk(nface(1)+1, nface(2)+1, nface(3)+1)
    integer(i8) :: i, j, k, ig, jg, kg
    integer(i8) :: nvert(3)
  continue
    nvert = nface - 1
  !...Compute gradient du/di in i direction
    do k = 1, nface(3) + 1
      do j = 1, nface(2) + 1
        do i = 1, nface(1)
          ig = i + dghost(1) - 1
          jg = j + dghost(2) - 1
          kg = k + dghost(3) - 1
          gradii(i,j,k) = (pg(ig+1,jg,kg) - pg(ig,jg,kg)) / dr(1)
        end do
      end do
    end do
  !...Compute gradient du/dj in j direction
    do k = 1, nface(3) + 1
      do i = 1, nface(1) + 1
        do j = 1, nface(2)
          ig = i + dghost(1) - 1
          jg = j + dghost(2) - 1
          kg = k + dghost(3) - 1
          gradjj(i,j,k) = (pg(ig,jg+1,kg) - pg(ig,jg,kg)) / dr(2)
        end do
      end do
    end do
  !...Compute gradient du/dk in k direction
    do j = 1, nface(2) + 1
      do i = 1, nface(1) + 1
        do k = 1, nface(3)
          ig = i + dghost(1) - 1
          jg = j + dghost(2) - 1
          kg = k + dghost(3) - 1
          gradkk(i,j,k) = (pg(ig,jg,kg+1) - pg(ig,jg,kg)) / dr(3)
        end do
      end do
    end do
  !...Compute gradient du/di, du/dj, du/dk
    select case (dir)
      case (1) !< i-direction face
        do k = 1, nvert(3)
          do j = 1, nvert(2)
            do i = 1, nface(1)
              grad(i,j,k,1) = gradii(i,j+1,k+1)
              grad(i,j,k,2) = 0.25_dp * (gradjj(i,j,  k+1) + gradjj(i+1,j,  k+1) + &
                                         gradjj(i,j+1,k+1) + gradjj(i+1,j+1,k+1))
              grad(i,j,k,3) = 0.25_dp * (gradkk(i,j+1,k)   + gradkk(i+1,j+1,k)   + &
                                         gradkk(i,j+1,k+1) + gradkk(i+1,j+1,k+1))
            end do
          end do
        end do
      case (2) !< j-direction face
        do k = 1, nvert(3)
          do i = 1, nvert(1)
            do j = 1, nface(2)
              grad(i,j,k,1) = 0.25_dp * (gradii(i  ,j,k+1) + gradii(i  ,j+1,k+1) + &
                                         gradii(i+1,j,k+1) + gradii(i+1,j+1,k+1))
              grad(i,j,k,2) = gradjj(i+1,j,k+1)
              grad(i,j,k,3) = 0.25_dp * (gradkk(i+1,j,k)   + gradkk(i+1,j+1,k)   + &
                                         gradkk(i+1,j,k+1) + gradkk(i+1,j+1,k+1))
            end do
          end do
        end do
      case (3) !< k-direction face
        do j = 1, nvert(2)
          do i = 1, nvert(1)
            do k = 1, nface(3)
              grad(i,j,k,1) = 0.25_dp * (gradii(i  ,j+1,k) + gradii(i  ,j+1,k+1) + &
                                         gradii(i+1,j+1,k) + gradii(i+1,j+1,k+1))
              grad(i,j,k,2) = 0.25_dp * (gradjj(i+1,j,  k) + gradjj(i+1,j,  k+1) + &
                                         gradjj(i+1,j+1,k) + gradjj(i+1,j+1,k+1))
              grad(i,j,k,3) = gradkk(i+1,j+1,k)
            end do
          end do
        end do

      case default
        call devlog%print(where="scheme_gradient_finite_diff::gradient_finite_diff_sub", &
                          message="Invalid wind direction",&
                          level=LogLevel%error)

    end select
  end subroutine gradient_finite_diff_sub

end module scheme_gradient_finite_diff

