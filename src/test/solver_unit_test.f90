!=============================================================================80
!
!> Description
!
!=============================================================================80

#define TEST_4

#ifdef TEST_ALL
#define TEST_1
#define TEST_2
#define TEST_3
#define TEST_4
#endif

module solver_unit_test
! #ifdef UNIT_TEST
  implicit none
  private
  public  :: unit_test_process
contains
  subroutine unit_test_process()
  continue
    write (*,*)
    write (*,'(A)') "==================== UNIT TEST PROCESS ===================="
    write (*,*)
#ifdef TEST_3
    call test_ClassStudent
#endif
#ifdef TEST_4
    call test_ClassGroup
#endif
    write (*,*)
    write (*,'(A)') "========================== Done ==========================="
    write (*,*)
    return
  end subroutine unit_test_process

#ifdef TEST_3
  subroutine test_ClassStudent()
    use test_class_constructor
    type(ClassStudent), pointer :: Bob
    ! type(ClassStudent) :: Nick
    ! type(ClassStudent) :: Alice
  continue
    Bob => ClassStudent(1, "Bob")
    write (*,*) "Checking Student: ", trim(Bob%name)
    ! Nick%name = "Bob"
    ! call new_ClassStudent_sub(2, "Nick", Nick)
    ! write (*,*) "Checking Student :", trim(Nick%name)
    ! Alice%name = "Eric"
    ! call Alice%init(3, "Alice")
    ! write (*,*) "Checking Student :", trim(Alice%name)
    deallocate(Bob)
  end subroutine test_ClassStudent
#endif

#ifdef TEST_4
  subroutine test_ClassGroup()
    use test_linklist
    type(ClassGroup) :: Group
  continue
    call Group%add("Bob")
    call Group%add("Alice")
    call Group%add("Eric")
  end subroutine test_ClassGroup
#endif
end module solver_unit_test
