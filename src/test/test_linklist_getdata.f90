module test_link_mod

  implicit none
  private
  public :: link, constructor

  type link
    !private
    class(*), pointer :: value => null()
    type(link), pointer :: next => null()
  contains

  procedure :: getValue
  procedure :: printLinks
  procedure :: nextLink
  procedure :: setNextLink

  end type link

  interface link
    module procedure constructor
  end interface

  contains

    function constructor(value,next)
      class(link), pointer :: constructor
      class(*) :: value
      class(link), pointer :: next
      allocate(constructor)
      constructor%next => next
      allocate(constructor%value, source=value)
    end function constructor

    function nextLink(this)
      class(link) :: this
      class(link), pointer :: nextLink
      nextLink => this%next
    end function nextLink

    subroutine setNextLink(this, next)
      class(link) :: this
      class(link), pointer :: next
      this%next => next
    end subroutine setNextLink

    function getValue(this)
      class(link) :: this
      class(*), pointer :: getValue
      getValue => this%value
    end function getValue

    subroutine printLink(this)
      implicit none
      class(link) :: this
      select type(v => this%value)
      type is (integer)
        print *, v
      type is (character(*))
        print *, v(1:1)
      type is (real)
        print *, v
      class default
        stop 'prinkLink: unexpected type'
      end select
    end subroutine printLink

    subroutine printLinks(this)
    implicit none
    class(link) :: this
    class(link), pointer :: curr

    call printLink(curr)
    curr => this%next
    do while(associated(curr))
      call printLink(curr)
      curr => curr%next
    enddo

    end subroutine

  end module test_link_mod

  module test_list_mod
    use test_link_mod
    private
    public :: list
    type list
       class(link),pointer :: firstLink => null() ! first link in list
       class(link),pointer :: lastLink => null()  ! last link in list
     contains
       procedure :: printValues ! print linked list
       procedure :: addInteger  ! add integer to linked list
       procedure :: addChar     ! add character to linked list
       procedure :: addReal     ! add real to linked list
       procedure :: addValue    ! add class(*) to linked list
       procedure :: firstValue  ! return value associated with firstLink
       procedure :: isEmpty     ! return true if list is empty
       generic :: add => addInteger, addChar, addReal
     end type list


  contains
  !
    subroutine printValues(this)
      class(list) :: this

      if (.not.this%isEmpty()) then
         call this%firstLink%printLinks()
      endif
    end subroutine printValues
  !
    subroutine addValue(this, value)
      use test_link_mod
      class(list) :: this
      class(*) :: value
      class(link), pointer :: newLink

      if (.not. associated(this%firstLink)) then
         this%firstLink => constructor(value, this%firstLink)
         this%lastLink => this%firstLink
      else
         newLink => constructor(value, this%lastLink%nextLink())
         call this%lastLink%setNextLink(newLink)
         this%lastLink => newLink
      end if

    end subroutine addValue
  !
    subroutine addInteger(this, value)
     class(list) :: this
      integer value
      class(*), allocatable :: v

      allocate(v,source=value)
      call this%addValue(v)
    end subroutine addInteger

    subroutine addChar(this, value)
      class(list) :: this
      character :: value
      class(*), allocatable :: v

      allocate(v,source=value)
      call this%addValue(v)
    end subroutine addChar

    subroutine addReal(this, value)
      class(list) :: this
      real value
      class(*), allocatable :: v

      allocate(v,source=value)
      call this%addValue(v)
    end subroutine addReal
  !
    function firstValue(this)
      class(list) :: this
      class(*), pointer :: firstValue

      firstValue => this%firstLink%getValue()

    end function firstValue
  !
    function isEmpty(this)
      class(list) :: this
      logical isEmpty

      if (associated(this%firstLink)) then
         isEmpty = .false.
      else
         isEmpty = .true.
      endif
    end function isEmpty

  end module test_list_mod
