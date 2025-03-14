!=============================================================================80
!
!> Construct a linklist container.
!
! https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=attributes-class-fortran-2003
! https://community.intel.com/t5/Intel-Fortran-Compiler/having-trouble-running-generic-linked-list-program/m-p/938771
!
!=============================================================================80

module container_linklist

  implicit none

!...Define scope
  private

  public  :: linklist

!...Declare Class
  !> construction of linklist
  !!
  !!   next: head => node => ... => end => null
  !!
  !!   prev: null <= head <= ... <= node <= end
  !!
  type :: linklist
    class(linklist), pointer, private :: head => null()
    class(linklist), pointer, private :: end  => null()
    class(linklist), pointer, private :: prev => null() !< previous node
    class(linklist), pointer, private :: next => null()
    class(linklist), pointer, private :: curr => null() !< current node
    class(*),        pointer, private :: data__ => null() !< node data
  contains
    procedure, public, pass :: push_front => push_front_fun
    procedure, public, pass :: push_back  => push_back_fun
    procedure, public, pass :: pop_front  => pop_front_sub
    procedure, public, pass :: pop_back   => pop_back_sub
    ! procedure, public, pass :: insert
    ! procedure, public, pass :: erase
    ! procedure, public, pass :: clear
    procedure, public, pass :: size       => size_fun
    procedure, public, pass :: empty      => empty_fun
    ! procedure, public, pass :: front
    ! procedure, public, pass :: back
    ! procedure, public, pass :: remove
    ! procedure, public, pass :: sort
    ! procedure, public, pass :: merge
    ! procedure, public, pass :: reverse
    ! procedure, public, pass :: begin
    ! procedure, public, pass :: end
    procedure, public, pass :: get_data   => get_data_fun
    procedure, public, pass :: cycle      => cycle_fun
    procedure, public, pass :: exit_cycle => exit_cycle_fun
    procedure, public, pass :: current    => current_fun

    final :: delete_linklist
  end type linklist
  !> Constructor of class linklist
  interface linklist
    module procedure new_linklist
  end interface linklist

contains

  !> Constructor of class linklist
  type(linklist) function new_linklist() result(this)
  continue
    this%head => null()
    this%end  => null()
    this%prev => null()
    this%next => null()
    this%curr => null()
    this%data__ => null()
  end function new_linklist

  !> Destructor of class linklist
  subroutine delete_linklist(this)
    type(linklist), intent(inout) :: this
  continue
    if (associated(this%next)) deallocate(this%next)
    if (associated(this%data__)) deallocate(this%data__)
  end subroutine delete_linklist

  !> Check if the list is empty
  logical function empty_fun(this)
    class(linklist), intent(inout) :: this
  continue
    if (associated(this%head)) then
      empty_fun = .false.
    else
      empty_fun = .true.
    end if
  end function empty_fun

  !> Add a node to the front of the list
  integer function push_front_fun(this, data__) result(stat_alloc)
    class(linklist), intent(inout) :: this
    class(*),        intent(in)    :: data__
  !...Declare local variables
    class(linklist), pointer       :: new_node
  continue
  !...Create new node
    allocate(new_node, stat=stat_alloc)
    if (stat_alloc /= 0) return
    allocate(new_node%data__, source=data__, stat=stat_alloc)
    if (stat_alloc /= 0) return
  !...Link head and node
    if (.not. this%empty()) then
    !...Link head and node
      this%head%prev => new_node
      new_node%next => this%head
    !...Update head
      this%head => new_node
    else
      this%head => new_node
      this%end  => new_node
      this%curr => new_node
    end if
    this%next => this%head
    nullify(new_node)
  end function push_front_fun

  !> Add a node to the back of the list
  integer function push_back_fun(this, data__) result(stat_alloc)
    use iso_c_binding
    class(linklist), intent(inout) :: this
    class(*),        intent(in)    :: data__
  !...Declare local variables
    class(linklist), pointer       :: new_node
  continue
  !...Create new node
    allocate(new_node, stat=stat_alloc)
    if (stat_alloc /= 0) return
    allocate(new_node%data__, source=data__, stat=stat_alloc)
    if (stat_alloc /= 0) return
  !...Link end and node
    if (.not. this%empty()) then
    !...Link end and node
      this%end%next => new_node
      new_node%prev => this%end
    !...Update end
      this%end => new_node
    else
      this%head => new_node
      this%end  => new_node
      this%curr => new_node
    end if
    this%next => this%head
    nullify(new_node)
  end function push_back_fun

  !> Remove a node from the front of the list
  subroutine pop_front_sub(this)
    class(linklist), intent(inout) :: this
    class(linklist), pointer :: tmp
  continue
    if (associated(this%head)) then
    !...Save head temporary
      tmp       => this%head
    !...disconnect head
      this%head => this%head%next
      if (associated(this%head)) then
        this%head%prev => null()
      else
        this%end => null()
      end if
    !...Deallocate head
      deallocate(tmp)
    end if
    this%next => this%head
  end subroutine pop_front_sub

  !> Remove a node from the back of the list
  subroutine pop_back_sub(this)
    class(linklist), intent(inout) :: this
    class(linklist), pointer       :: tmp
  continue
    if (associated(this%end)) then
    !...Save end temporary
      tmp       => this%end
    !...disconnect end
      this%end => this%end%prev
      if (associated(this%end)) then
        this%end%next => null()
      else
        this%head => null()
      end if
    !...Deallocate end
      deallocate(tmp)
    end if
    this%next => this%head
  end subroutine pop_back_sub

  !> Get the size of the list
  integer function size_fun(this)
    class(linklist), intent(inout) :: this
    class(linklist), pointer       :: tmp
  continue
    size_fun = 0
    if (.not. this%empty()) then
      tmp => this%head
      do while (associated(tmp))
        size_fun = size_fun + 1
        tmp => tmp%next
      end do
    end if
  end function size_fun

  !> Get the value of a node
  function get_data_fun(this, index) result(data__)
    class(linklist), intent(inout) :: this
    integer,         intent(in)    :: index
    class(*),        pointer       :: data__
  !...Declare local variables
    integer                        :: i
  continue
    if ( this%empty() ) return
    if ( index > this%size() ) return
    i = 1
    this%curr => this%head
    do while (i < index)
      this%curr => this%curr%next
      i = i + 1
    end do
    data__ => this%curr%data__
    this%curr => this%head
  end function get_data_fun

  !> Cycle through the list
  logical function cycle_fun(this)
    class(linklist), intent(inout) :: this
  continue
    cycle_fun = .false.
    if (associated(this%curr)) then
      cycle_fun = .true.
    else
      this%curr => this%head
    end if
  end function cycle_fun

  !> Exit the cycle
  subroutine exit_cycle_fun(this)
    class(linklist), intent(inout) :: this
  continue
    this%curr => this%head
  end subroutine exit_cycle_fun

  !> Get the value of the current node
  function current_fun(this) result(data__)
    class(linklist), intent(inout) :: this
    class(*),        pointer       :: data__
  continue
    if ( associated(this%curr) ) then
      data__ => this%curr%data__
      this%curr => this%curr%next
    else
      data__ => null()
    end if
  end function current_fun

end module container_linklist
