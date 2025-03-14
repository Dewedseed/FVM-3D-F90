!=============================================================================80
!
!> Initial condition.
!
!=============================================================================80

module class_init_cond

  use kind_parameter, only : dp, len_short

  use container_linklist, only : linklist

  implicit none

!...Define scope
  private

!...Declare Class
  type :: ClassGeoSquare
    character(len_short), private :: type = "square"
  end type ClassGeoSquare

  type :: ClassInitCondData
    integer, private :: var1
    integer, public  :: var2
    ! final :: delete_ClassInitCondData
  end type ClassInitCondData

  type :: ClassInitCond
    integer, private :: nCond = 0
    type(linklist), private :: geo    !< contain geometry
    type(linklist), private :: data   !< contain data
  contains
  end type ClassInitCond

contains

end module class_init_cond
