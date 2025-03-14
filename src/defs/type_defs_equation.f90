module type_defs_equation

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassEquationType
    integer, public :: NULL   = -1
    integer, public :: Euler  = 0
    integer, public :: NS     = 1
  !...Equation types
    integer, public :: Mass        = 101
    integer, public :: MomentumX   = 102
    integer, public :: MomentumY   = 103
    integer, public :: MomentumZ   = 104
    integer, public :: Energy      = 105
    integer, public :: TurbKinetic = 106
    integer, public :: TurbDissp   = 107
    integer, public :: Species     = 108
    integer, public :: Chemistry   = 109
    integer, public :: UDF         = 100 !< User-Defined
  !...Term types
    integer, public :: TermTimeDep  = 201
    integer, public :: TermInviscid = 202
    integer, public :: TermViscous  = 203
    integer, public :: TermSource   = 204
  !...Dependent Variable types
    integer, public :: VarDensity     = 301
    integer, public :: VarVelocityX   = 302
    integer, public :: VarVelocityY   = 303
    integer, public :: VarVelocityZ   = 304
    integer, public :: VarEnergy      = 305
    integer, public :: VarTurbKinetic = 306
    integer, public :: VarTurbDissp   = 307
    integer, public :: VarSpecies     = 308
    integer, public :: VarUDF         = 300 !< User-Defined
    integer, public :: VarNull        = -300
  !...Dependent residual types
    integer, public :: ResMass        = 401
    integer, public :: ResMomentumX   = 402
    integer, public :: ResMomentumY   = 403
    integer, public :: ResMomentumZ   = 404
    integer, public :: ResEnergy      = 405
    integer, public :: ResTurbKinetic = 406
    integer, public :: ResTurbDissp   = 407
    integer, public :: ResSpecies     = 408
    integer, public :: ResUDF         = 400 !< User-Defined
    integer, public :: ResNull        = -400
  end type ClassEquationType

contains

  !> Get equation index
  integer function EquationIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassEquationType)     :: EquationType
  continue
    select case (trim(name))
      case ("Euler")
        index = EquationType%Euler
      case ("NavierStokes")
        index = EquationType%NS
      case ("mass")
        index = EquationType%Mass
      case ("momentumX")
        index = EquationType%MomentumX
      case ("momentumY")
        index = EquationType%MomentumY
      case ("momentumZ")
        index = EquationType%MomentumZ
      case ("energy")
        index = EquationType%Energy
      case ("turbKinetic")
        index = EquationType%TurbKinetic
      case ("turbDissp")
        index = EquationType%TurbDissp
      case default
        index = EquationType%NULL
    end select
  end function EquationIndex_fun

  !> Get equation name
  character(len_name) function EquationName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassEquationType) :: EquationType
  continue
    if ( index == EquationType%Euler ) then
      name = "Euler"
    else if ( index == EquationType%NS ) then
      name = "NavierStokes"
    else if ( index == EquationType%Mass ) then
      name = "mass"
    else if ( index == EquationType%MomentumX ) then
      name = "momentumX"
    else if ( index == EquationType%MomentumY ) then
      name = "momentumY"
    else if ( index == EquationType%MomentumZ ) then
      name = "momentumZ"
    else if ( index == EquationType%Energy ) then
      name = "energy"
    else if ( index == EquationType%TurbKinetic ) then
      name = "turbKinetic"
    else if ( index == EquationType%TurbDissp ) then
      name = "turbDissp"
    else
      name = "Null"
    end if
  end function EquationName_fun

  !> Get equation dependent variable index
  integer function EqnVarIndex_fun(eqn_type) result(index)
    integer,     intent(in) :: eqn_type
    type(ClassEquationType) :: EquationType
  continue
    if ( eqn_type == EquationType%Mass ) then
      index = EquationType%VarDensity
    else if ( eqn_type == EquationType%MomentumX ) then
      index = EquationType%VarVelocityX
    else if ( eqn_type == EquationType%MomentumY ) then
      index = EquationType%VarVelocityY
    else if ( eqn_type == EquationType%MomentumZ ) then
      index = EquationType%VarVelocityZ
    else if ( eqn_type == EquationType%Energy ) then
      index = EquationType%VarEnergy
    else if ( eqn_type == EquationType%TurbKinetic ) then
      index = EquationType%VarTurbKinetic
    else if ( eqn_type == EquationType%TurbDissp ) then
      index = EquationType%VarTurbDissp
    else
      index = EquationType%VarNull
    end if
  end function EqnVarIndex_fun

  !> Get equation residual index
  elemental integer function EqnResIndex_fun(eqn_type) result(index)
    integer,     intent(in) :: eqn_type
    type(ClassEquationType) :: EquationType
  continue
    if ( eqn_type == EquationType%Mass ) then
      index = EquationType%ResMass
    else if ( eqn_type == EquationType%MomentumX ) then
      index = EquationType%ResMomentumX
    else if ( eqn_type == EquationType%MomentumY ) then
      index = EquationType%ResMomentumY
    else if ( eqn_type == EquationType%MomentumZ ) then
      index = EquationType%ResMomentumZ
    else if ( eqn_type == EquationType%Energy ) then
      index = EquationType%ResEnergy
    else if ( eqn_type == EquationType%TurbKinetic ) then
      index = EquationType%ResTurbKinetic
    else if ( eqn_type == EquationType%TurbDissp ) then
      index = EquationType%ResTurbDissp
    else
      index = EquationType%ResNull
    end if
  end function EqnResIndex_fun

  !> Get equation residual name
  elemental character(len_name) function EqnResName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassEquationType) :: EquationType
  continue
    if ( index == EquationType%ResMass ) then
      name = "R_mass"
    else if ( index == EquationType%ResMomentumX ) then
      name = "R_momx"
    else if ( index == EquationType%ResMomentumY ) then
      name = "R_momy"
    else if ( index == EquationType%ResMomentumZ ) then
      name = "R_momz"
    else if ( index == EquationType%ResEnergy ) then
      name = "R_energy"
    else if ( index == EquationType%ResTurbKinetic ) then
      name = "R_turbk"
    else if ( index == EquationType%ResTurbDissp ) then
      name = "R_eps"
    else
      name = "Null"
    end if
  end function EqnResName_fun

end module type_defs_equation
