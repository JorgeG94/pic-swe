module test_helpers
   use pic_types, only: sp, dp
   implicit none
   real(dp), parameter :: tol_dp = 1.0e-12_dp
   real(sp), parameter :: tol_sp = 1.0e-6_sp
   private
   public ::  print_matrix_to_fortran

contains

   subroutine print_matrix_to_fortran(mat, name)
   !! Prints matrix as a real(dp), parameter :: name(dim1, dim2) = reshape([...], [dim1, dim2])
      real(dp), intent(in) :: mat(:, :)
      character(len=*), intent(in) :: name
      integer :: i, j, nx, ny

      nx = size(mat, 1)
      ny = size(mat, 2)

      print *, "real(dp), parameter :: ", trim(name), "(", nx, ",", ny, ") = reshape([ &"
      do j = 1, ny
         do i = 1, nx
            if (i == nx .and. j == ny) then
               write (*, '(F10.6,"_dp")', advance='no') mat(i, j)
            else
               write (*, '(F10.6,"_dp,",1x)', advance='no') mat(i, j)
            end if
         end do
      end do
      print *, " &], [", nx, ",", ny, "])"

   end subroutine print_matrix_to_fortran

end module test_helpers

module test_suite1
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   public :: collect_suite1

contains

!> Collect all exported unit tests
   subroutine collect_suite1(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2

      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("valid", test_valid)
      testsuite(2) = new_unittest("invalid", test_invalid, should_fail=.true.)

   end subroutine collect_suite1

   subroutine test_valid(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, 1 + 2 == 3)
      if (allocated(error)) return

   end subroutine test_valid

   subroutine test_invalid(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: i
      ! ...
      i = 1

      if (i == 1) then
         call test_failed(error, "Custom check failed")
         return
      end if
   end subroutine test_invalid

end module test_suite1
