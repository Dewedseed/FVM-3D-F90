module type_defs_scheme

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassSchemeType
    integer, public :: NULL = -1
  !...Temporal discretization schemes
    integer, public :: Runge_Kutta = 101
    integer, public :: BDF1        = 102
    integer, public :: BDF2        = 103
    integer, public :: BDF3        = 104
    integer, public :: MEBDF4      = 105
    integer, public :: SSPRK3      = 106
  !...Spatial discretization schemes
    ! Reconstruction schemes
    integer, public :: simple      = 200
    integer, public :: MUSCL       = 201
    integer, public :: WENO        = 202
    integer, public :: WCNS        = 203
    ! Reconstruction limiter schemes
    integer, public :: limiter_none      = 250
    integer, public :: limiter_minmod    = 251
    integer, public :: limiter_vanLeer   = 252
    integer, public :: limiter_vanAlbada = 253
    ! Inviscid flux split schemes
    integer, public :: Roe         = 300
    integer, public :: VanLeer     = 301
    integer, public :: HLLC        = 302
    integer, public :: AUSM        = 303
  end type ClassSchemeType

contains

  !> Get scheme index
  integer function SchemeIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassSchemeType)     :: SchemeType
  continue
    if ( trim(name) == "Runge-Kutta" ) then
      index = SchemeType%Runge_Kutta
    else if ( trim(name) == "BDF1" ) then
      index = SchemeType%BDF1
    else if ( trim(name) == "BDF2" ) then
      index = SchemeType%BDF2
    else if ( trim(name) == "BDF3" ) then
      index = SchemeType%BDF3
    else if ( trim(name) == "MEBDF4" ) then
      index = SchemeType%MEBDF4
    else if ( trim(name) == "SSPRK3" ) then
      index = SchemeType%SSPRK3
    else if ( trim(name) == "simple" ) then
      index = SchemeType%simple
    else if ( trim(name) == "MUSCL" ) then
      index = SchemeType%MUSCL
    else if ( trim(name) == "WENO" ) then
      index = SchemeType%WENO
    else if ( trim(name) == "WCNS" ) then
      index = SchemeType%WCNS
    else if ( trim(name) == "none_" ) then
      index = SchemeType%limiter_none
    else if ( trim(name) == "minmod_" ) then
      index = SchemeType%limiter_minmod
    else if ( trim(name) == "VanLeer_" ) then
      index = SchemeType%limiter_vanLeer
    else if ( trim(name) == "vanAlbada_" ) then
      index = SchemeType%limiter_vanAlbada
    else if ( trim(name) == "Roe" ) then
      index = SchemeType%Roe
    else if ( trim(name) == "VanLeer" ) then
      index = SchemeType%VanLeer
    else if ( trim(name) == "HLLC" ) then
      index = SchemeType%HLLC
    else if ( trim(name) == "AUSM" ) then
      index = SchemeType%AUSM
    else
      index = SchemeType%NULL
    end if
  end function SchemeIndex_fun

  !> Get scheme name
  character(len_name) function SchemeName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassSchemeType) :: SchemeType
    continue
    if ( index == SchemeType%Runge_Kutta ) then
      name = "Runge-Kutta"
    else if ( index == SchemeType%BDF1 ) then
      name = "1st-order backward difference"
    else if ( index == SchemeType%BDF2 ) then
      name = "2nd-order backward difference"
    else if ( index == SchemeType%BDF3 ) then
      name = "3rd-order backward difference"
    else if ( index == SchemeType%MEBDF4 ) then
      name = "4th-order modified extended backward difference"
    else if ( index == SchemeType%SSPRK3 ) then
      name = "3rd-order Strong Stability Preserving Runge-Kutta"
    else if ( index == SchemeType%simple ) then
      name = "simple"
    else if ( index == SchemeType%MUSCL ) then
      name = "MUSCL"
    else if ( index == SchemeType%limiter_none ) then
      name = "limiter_none"
    else if ( index == SchemeType%limiter_minmod ) then
      name = "limiter_minmod"
    else if ( index == SchemeType%limiter_vanLeer ) then
      name = "limiter_vanLeer"
    else if ( index == SchemeType%limiter_vanAlbada ) then
      name = "limiter_vanAlbada"
    else if ( index == SchemeType%Roe ) then
      name = "Roe"
    else if ( index == SchemeType%VanLeer ) then
      name = "VanLeer"
    else if ( index == SchemeType%WENO ) then
      name = "WENO"
    else
      name = "NULL"
    end if
  end function SchemeName_fun

end module type_defs_scheme
