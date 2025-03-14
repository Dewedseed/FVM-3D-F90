!=============================================================================80
!
!> Variable name: CGNS Data-Name Identifiers for Flow Solution Quantities
!    https://cgns.github.io/standard/SIDS/appendix_a.html#a2-flowfield-solution
!
!=============================================================================80

module type_defs_variable

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassVariableType
    integer, public :: NULL = -1
  !...Geometry variables
    integer, public :: X = 101
    integer, public :: Y = 102
    integer, public :: Z = 103
  !...Primitive variables
    integer, public :: Density    = 201
    integer, public :: Velocity_X = 202
    integer, public :: Velocity_Y = 203
    integer, public :: Velocity_Z = 204
  !...Static variables
    integer, public :: StaticPressure    = 301
    integer, public :: StaticTemperature = 302
  !...Dimensionless variables
    integer, public :: Mach    = 901
    integer, public :: Gamma   = 902
  end type ClassVariableType

contains

  !> Get variable index
  integer function VariableIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassVariableType)     :: VariableType
  continue
    if ( trim(name) == "X" ) then
      index = VariableType%X
    else if ( trim(name) == "Y" ) then
      index = VariableType%Y
    else if ( trim(name) == "Z" ) then
      index = VariableType%Z
    else if ( trim(name) == "Density" ) then
      index = VariableType%Density
    else if ( trim(name) == "VelocityX" ) then
      index = VariableType%Velocity_X
    else if ( trim(name) == "VelocityY" ) then
      index = VariableType%Velocity_Y
    else if ( trim(name) == "VelocityZ" ) then
      index = VariableType%Velocity_Z
    else if ( trim(name) == "Pressure" ) then
      index = VariableType%StaticPressure
    else if ( trim(name) == "Temperature" ) then
      index = VariableType%StaticTemperature
    else
      index = VariableType%NULL
    end if
  end function VariableIndex_fun

  !> Get variable name
  character(len_name) function VariableName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassVariableType) :: VariableType
  continue
    if ( index == VariableType%X ) then
      name = "X"
    else if ( index == VariableType%Y ) then
      name = "Y"
    else if ( index == VariableType%Z ) then
      name = "Z"
    else if ( index == VariableType%Density ) then
      name = "Density"
    else if ( index == VariableType%Velocity_X ) then
      name = "VelocityX"
    else if ( index == VariableType%Velocity_Y ) then
      name = "VelocityY"
    else if ( index == VariableType%Velocity_Z ) then
      name = "VelocityZ"
    else if ( index == VariableType%StaticPressure ) then
      name = "Pressure"
    else if ( index == VariableType%StaticTemperature ) then
      name = "Temperature"
    else
      name = "Null"
    end if
  end function VariableName_fun

end module type_defs_variable
