program polymorphic_example
  type :: base
      integer :: i
  end type base

  type, extends(base) :: child
      integer :: j
  end type child

  class(*), allocatable :: anyThing

  type(child) :: child1
  type(base) :: base1

  allocate(anyThing, source=child1)
  call printAny(anyThing)

  allocate(anyThing, source=base1)
  call printAny(anyThing)

contains
  subroutine printAny(item)
      class(*), intent(in) :: item
      select type(item)
          type is (base)
              print *, 'Base item with i = ', item%i
          type is (child)
              print *, 'Child item with i = ', item%i, ' and j = ', item%j
          class default
              print *, 'Unknown type'
      end select
  end subroutine printAny
end program polymorphic_example
