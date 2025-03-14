module type_defs_dimension

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassDimensionType
    integer, public :: NULL = -1
    integer, public :: DIM0 = 0
    integer, public :: DIM1 = 1
    integer, public :: DIM2 = 2
    integer, public :: DIM3 = 3
  end type ClassDimensionType

contains

  !> Get dimension index
  integer function DimensionIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassDimensionType)     :: DimensionType
  continue
    if ( trim(name) == "0D" ) then
      index = DimensionType%DIM0
    else if ( trim(name) == "1D" ) then
      index = DimensionType%DIM1
    else if ( trim(name) == "2D" ) then
      index = DimensionType%DIM2
    else if ( trim(name) == "3D" ) then
      index = DimensionType%DIM3
    else
      index = DimensionType%NULL
    end if
  end function DimensionIndex_fun

  !> Get dimension name
  character(len_name) function DimensionName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassDimensionType) :: DimensionType
    continue
    if ( index == DimensionType%DIM0 ) then
      name = "0D"
    else if ( index == DimensionType%DIM1 ) then
      name = "1D"
    else if ( index == DimensionType%DIM2 ) then
      name = "2D"
    else if ( index == DimensionType%DIM3 ) then
      name = "3D"
    else
      name = "NULL"
    end if
  end function DimensionName_fun

end module type_defs_dimension
