module my_module
  implicit none

  type :: my_type
    contains
      procedure :: my_procedure
  end type my_type

  interface my_procedure_new
    subroutine my_procedure(self)
      import :: my_type
      class(my_type), intent(inout) :: self
    end subroutine my_procedure
  end interface my_procedure_new

contains


end module my_module
