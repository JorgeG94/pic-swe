module test_grid_2d
   use pic_test_helpers, only: is_equal
   use pic_types, only: dp
   use pic_swe_grid_2d, only: grid_2d_type, init_grid, generate_2d_grids
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   implicit none
   private

   public :: collect_tests_grid_2d

contains

   subroutine collect_tests_grid_2d(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2

      print *, "INITIALIZING"
      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_init_grid", test_init_grid)
      testsuite(2) = new_unittest("test_init_grid_dx", test_init_grid_dx)

   end subroutine collect_tests_grid_2d

   subroutine test_init_grid(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx, dy

      xmin = 0.0_dp
      xmax = 10.0_dp
      ymin = 0.0_dp
      ymax = 5.0_dp
      dx = 1.0_dp
      dy = 1.0_dp

      call init_grid(grid, xmin, xmax, ymin, ymax, dx, dy)

      print *, grid%xmin

      call check(error, is_equal(grid%xmin, xmin))
      call check(error, is_equal(grid%xmax, xmax))
      call check(error, is_equal(grid%ymin, ymin))
      call check(error, is_equal(grid%ymax, ymax))
      call check(error, is_equal(grid%dx, dx))
      call check(error, is_equal(grid%dy, dy))
      call check(error, grid%nx == 11 .and. grid%ny == 6)

      if (allocated(error)) return

      call generate_2d_grids(grid)
   end subroutine test_init_grid

   subroutine test_init_grid_dx(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx

      xmin = 0.0_dp
      xmax = 10.0_dp
      ymin = 0.0_dp
      ymax = 5.0_dp
      dx = 1.0_dp

      call init_grid(grid, xmin, xmax, ymin, ymax, dx)

      call check(error, is_equal(grid%xmin, xmin))
      call check(error, is_equal(grid%xmax, xmax))
      call check(error, is_equal(grid%ymin, ymin))
      call check(error, is_equal(grid%ymax, ymax))
      call check(error, is_equal(grid%dx, dx))
      call check(error, grid%nx == 11 .and. grid%ny == 6)

   end subroutine test_init_grid_dx
end module test_grid_2d
