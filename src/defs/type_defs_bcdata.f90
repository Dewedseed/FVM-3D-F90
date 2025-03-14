module type_defs_bcdata

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassBCDataType
    integer, public :: NULL      = -1
    integer, public :: Dirichlet = 11
    integer, public :: Neumann   = 12
    integer, public :: Robin     = 13
  end type ClassBCDataType

contains

  !> Get BCDataType index
  integer function BCDataTypeIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassBCDataType)     :: BCDataType
  continue
    if ( trim(name) == "Dirichlet" ) then
      index = BCDataType%Dirichlet
    else if ( trim(name) == "Neumann" ) then
      index = BCDataType%Neumann
    else if ( trim(name) == "Robin" ) then
      index = BCDataType%Robin
    else
      index = BCDataType%NULL
    end if
  end function BCDataTypeIndex_fun

  !> Get BCDataType name
  character(len_name) function BCDataTypeName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassBCDataType) :: BCDataType
  continue
    if ( index == BCDataType%Dirichlet ) then
      name = "Dirichlet"
    else if ( index == BCDataType%Neumann ) then
      name = "Neumann"
    else if ( index == BCDataType%Robin ) then
      name = "Robin"
    else
      name = "NULL"
    end if
  end function BCDataTypeName_fun

end module type_defs_bcdata
