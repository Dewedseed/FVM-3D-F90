module type_defs_file

  implicit none

!...Define scope
  public

!...Declare class
  integer, parameter :: len_name = 32

  type :: ClassFileType
    integer, public :: NULL     = -1
    integer, public :: Namelist = 0
    integer, public :: CGNS     = 1
    integer, public :: Tecplot  = 2
  end type ClassFileType

contains

  !> Get file index
  integer function FileIndex_fun(name) result(index)
    character(len=*), intent(in) :: name
    type(ClassFileType)     :: FileType
  continue
    if ( trim(name) == "Namelist" ) then
      index = FileType%Namelist
    else if ( trim(name) == "CGNS" ) then
      index = FileType%CGNS
    else if ( trim(name) == "Tecplot" ) then
      index = FileType%Tecplot
    else
      index = FileType%NULL
    end if
  end function FileIndex_fun

  !> Get file name
  character(len_name) function FileName_fun(index) result(name)
    integer,      intent(in) :: index
    type(ClassFileType) :: FileType
  continue
    if ( index == FileType%Namelist ) then
      name = "Namelist"
    else if ( index == FileType%CGNS ) then
      name = "CGNS"
    else if ( index == FileType%Tecplot ) then
      name = "Tecplot"
    else
      name = "Null"
    end if
  end function FileName_fun

  !> Get file type
  integer function FileType_fun(suffix) result(index)
    character(len=*), intent(in) :: suffix
    type(ClassFileType)     :: FileType
  continue
    if ( trim(suffix) == ".nml" ) then
      index = FileType%Namelist
    else if ( trim(suffix) == ".cgns" ) then
      index = FileType%CGNS
    else if ( trim(suffix) == ".tec" ) then
      index = FileType%Tecplot
    else
      index = FileType%NULL
    end if
  end function FileType_fun

  !> Get file suffix
  function FileExt_fun(index) result(suffix)
    integer,           intent(in) :: index
    character(len=:), allocatable :: suffix
    type(ClassFileType) :: FileType
  continue
    if ( index == FileType%Namelist ) then
      suffix = ".nml"
    else if ( index == FileType%CGNS ) then
      suffix = ".cgns"
    else if ( index == FileType%Tecplot ) then
      suffix = ".tec"
    else
      suffix = ".null"
    end if
  end function FileExt_fun

end module type_defs_file
