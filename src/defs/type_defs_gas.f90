module type_defs_gas

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassGasType
    integer, public :: NULL   = -1
    integer, public :: Calorically_Perfect = 0
    integer, public :: Calorically         = 1
    integer, public :: Thermally_Perfect   = 2
    integer, public :: Thermally           = 3
  end type ClassGasType

contains

  !> Get gas index
  integer function GasIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassGasType)     :: GasType
  continue
    if ( trim(name) == "Calorically Perfect" ) then
      index = GasType%Calorically_Perfect
    else if ( trim(name) == "ideal" ) then
      index = GasType%Calorically_Perfect
    else if ( trim(name) == "Calorically" ) then
      index = GasType%Calorically
    else if ( trim(name) == "Thermally Perfect" ) then
      index = GasType%Thermally_Perfect
    else if ( trim(name) == "Thermally" ) then
      index = GasType%Thermally
    else
      index = GasType%NULL
    end if
  end function GasIndex_fun

  !> Get gas name
  character(len_name) function GasName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassGasType) :: GasType
  continue
    if ( index == GasType%Calorically_Perfect ) then
      name = "Calorically Perfect"
    else if ( index == GasType%Calorically ) then
      name = "Calorically"
    else if ( index == GasType%Thermally_Perfect ) then
      name = "Thermally Perfect"
    else if ( index == GasType%Thermally ) then
      name = "Thermally"
    else
      name = "Null"
    end if
  end function GasName_fun

end module type_defs_gas
