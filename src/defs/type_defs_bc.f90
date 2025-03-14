module type_defs_bc

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassBCType
    integer, public :: NULL                  = -1
    integer, public :: UserDefined           = -2
    integer, public :: Connectivity          = 0
    integer, public :: Boundary              = 1
    integer, public :: Inflow                = 10
    integer, public :: InflowSubsonic        = 11
    integer, public :: InflowSupersonic      = 12
    integer, public :: Outflow               = 20
    integer, public :: OutflowSubsonic       = 21
    integer, public :: OutflowSupersonic     = 22
    integer, public :: Wall                  = 30
    integer, public :: WallInviscid          = 31
    integer, public :: WallViscous           = 33
    integer, public :: WallViscousIsothermal = 32
    integer, public :: Farfield              = 40
    integer, public :: Extrapolate           = 41
    integer, public :: Symmetry              = 50
    integer, public :: AxisymmetricWedge     = 51
  end type ClassBCType

contains

  !> Get dimension index
  integer function BCTypeIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassBCType)            :: BCType
  continue
    if ( trim(name) == "UserDefined" ) then
      index = BCType%UserDefined
    else if ( trim(name) == "Connectivity" ) then
      index = BCType%Connectivity
    else if ( trim(name) == "Boundary" ) then
      index = BCType%Boundary
    else if ( trim(name) == "Inflow" ) then
      index = BCType%Inflow
    else if ( trim(name) == "InflowSubsonic" ) then
      index = BCType%InflowSubsonic
    else if ( trim(name) == "InflowSupersonic" ) then
      index = BCType%InflowSupersonic
    else if ( trim(name) == "Outflow" ) then
      index = BCType%Outflow
    else if ( trim(name) == "OutflowSubsonic" ) then
      index = BCType%OutflowSubsonic
    else if ( trim(name) == "OutflowSupersonic" ) then
      index = BCType%OutflowSupersonic
    else if ( trim(name) == "Wall" ) then
      index = BCType%Wall
    else if ( trim(name) == "WallInviscid" ) then
      index = BCType%WallInviscid
    else if ( trim(name) == "WallViscous" ) then
      index = BCType%WallViscous
    else if ( trim(name) == "WallViscousIsothermal" ) then
      index = BCType%WallViscousIsothermal
    else if ( trim(name) == "Farfield" ) then
      index = BCType%Farfield
    else if ( trim(name) == "Extrapolate" ) then
      index = BCType%Extrapolate
    else if ( trim(name) == "Symmetry" ) then
      index = BCType%Symmetry
    else if ( trim(name) == "AxisymmetricWedge" ) then
      index = BCType%AxisymmetricWedge
    else
      index = BCType%NULL
    end if
  end function BCTypeIndex_fun

  !> Get dimension name
  character(len_name) function BCTypeName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassBCType)        :: BCType
  continue
    if ( index == BCType%UserDefined ) then
      name = "UserDefined"
    else if ( index == BCType%Connectivity ) then
      name = "Connectivity"
    else if ( index == BCType%Boundary ) then
      name = "Boundary"
    else if ( index == BCType%Inflow ) then
      name = "Inflow"
    else if ( index == BCType%InflowSubsonic ) then
      name = "InflowSubsonic"
    else if ( index == BCType%InflowSupersonic ) then
      name = "InflowSupersonic"
    else if ( index == BCType%Outflow ) then
      name = "Outflow"
    else if ( index == BCType%OutflowSubsonic ) then
      name = "OutflowSubsonic"
    else if ( index == BCType%OutflowSupersonic ) then
      name = "OutflowSupersonic"
    else if ( index == BCType%Wall ) then
      name = "Wall"
    else if ( index == BCType%WallInviscid ) then
      name = "WallInviscid"
    else if ( index == BCType%WallViscous ) then
      name = "WallViscous"
    else if ( index == BCType%WallViscousIsothermal ) then
      name = "WallViscousIsothermal"
    else if ( index == BCType%Farfield ) then
      name = "Farfield"
    else if ( index == BCType%Extrapolate ) then
      name = "Extrapolate"
    else if ( index == BCType%Symmetry ) then
      name = "Symmetry"
    else if ( index == BCType%AxisymmetricWedge ) then
      name = "AxisymmetricWedge"
    else
      name = "NULL"
    end if
  end function BCTypeName_fun

end module type_defs_bc
