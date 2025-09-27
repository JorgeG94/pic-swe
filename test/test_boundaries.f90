module test_boundaries
   use pic_types, only: dp
   use pic_swe_grid_2d, only: grid_2d_type, init_grid, generate_2d_grids
   use pic_swe_state_2d, only: state_2d_type, initialize_state, initialize_dam_break
   use pic_swe_boundaries, only: apply_reflective_boundaries
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use pic_test_helpers, only: is_equal
   implicit none
   private

   public :: collect_tests_boundaries

contains

   subroutine collect_tests_boundaries(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 1

      allocate (testsuite(ntests))

      testsuite(1) = new_unittest("test_apply_reflective_boundaries", test_apply_reflective_boundaries)
   end subroutine collect_tests_boundaries

   subroutine test_apply_reflective_boundaries(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx
      type(state_2d_type) :: state

      ! Initialize grid
      xmin = 0.0_dp
      xmax = 2.0_dp
      ymin = 0.0_dp
      ymax = 1.0_dp
      dx = 1.0_dp
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)
      call initialize_state(state, grid)

      block
         integer :: i, j
         integer :: nx, ny

         nx = grid%nx
         ny = grid%ny

         ! Fill the interior with predictable dummy values
         do i = 1, nx
            do j = 1, ny
               state%water_height(i, j) = 1.0_dp
               state%x_momentum(i, j) = real(i, dp)
               state%y_momentum(i, j) = real(j, dp)
            end do
         end do

         call apply_reflective_boundaries(state)

         ! === X direction boundaries ===
         do j = 1, ny
            call check(error, is_equal(state%water_height(1, j), state%water_height(2, j)), &
                       "X lower boundary: water_height")
            call check(error, is_equal(state%x_momentum(1, j), -state%x_momentum(2, j)), &
                       "X lower boundary: x_momentum")
            call check(error, is_equal(state%y_momentum(1, j), state%y_momentum(2, j)), &
                       "X lower boundary: y_momentum")

            call check(error, is_equal(state%water_height(nx, j), state%water_height(nx - 1, j)), &
                       "X upper boundary: water_height")
            call check(error, is_equal(state%x_momentum(nx, j), -state%x_momentum(nx - 1, j)), &
                       "X upper boundary: x_momentum")
            call check(error, is_equal(state%y_momentum(nx, j), state%y_momentum(nx - 1, j)), &
                       "X upper boundary: y_momentum")
         end do

         ! === Y direction boundaries ===
         do i = 1, nx
            call check(error, is_equal(state%water_height(i, 1), state%water_height(i, 2)), &
                       "Y lower boundary: water_height")
            call check(error, is_equal(state%x_momentum(i, 1), state%x_momentum(i, 2)), &
                       "Y lower boundary: x_momentum")
            call check(error, is_equal(state%y_momentum(i, 1), -state%y_momentum(i, 2)), &
                       "Y lower boundary: y_momentum")

            call check(error, is_equal(state%water_height(i, ny), state%water_height(i, ny - 1)), &
                       "Y upper boundary: water_height")
            call check(error, is_equal(state%x_momentum(i, ny), state%x_momentum(i, ny - 1)), &
                       "Y upper boundary: x_momentum")
            call check(error, is_equal(state%y_momentum(i, ny), -state%y_momentum(i, ny - 1)), &
                       "Y upper boundary: y_momentum")
         end do

         ! === Corners ===
         call check(error, is_equal(state%water_height(1, 1), state%water_height(2, 2)), "Corner (1,1) water_height")
         call check(error, is_equal(state%x_momentum(1, 1), -state%x_momentum(2, 2)), "Corner (1,1) x_momentum")
         call check(error, is_equal(state%y_momentum(1, 1), -state%y_momentum(2, 2)), "Corner (1,1) y_momentum")

         call check(error, is_equal(state%water_height(1, ny), state%water_height(2, ny - 1)), "Corner (1,ny) water_height")
         call check(error, is_equal(state%x_momentum(1, ny), -state%x_momentum(2, ny - 1)), "Corner (1,ny) x_momentum")
         call check(error, is_equal(state%y_momentum(1, ny), -state%y_momentum(2, ny - 1)), "Corner (1,ny) y_momentum")

         call check(error, is_equal(state%water_height(nx, 1), state%water_height(nx - 1, 2)), "Corner (nx,1) water_height")
         call check(error, is_equal(state%x_momentum(nx, 1), -state%x_momentum(nx - 1, 2)), "Corner (nx,1) x_momentum")
         call check(error, is_equal(state%y_momentum(nx, 1), -state%y_momentum(nx - 1, 2)), "Corner (nx,1) y_momentum")

         call check(error, is_equal(state%water_height(nx, ny), state%water_height(nx - 1, ny - 1)), "Corner (nx,ny) water_height")
         call check(error, is_equal(state%x_momentum(nx, ny), -state%x_momentum(nx - 1, ny - 1)), "Corner (nx,ny) x_momentum")
         call check(error, is_equal(state%y_momentum(nx, ny), -state%y_momentum(nx - 1, ny - 1)), "Corner (nx,ny) y_momentum")
      end block

   end subroutine test_apply_reflective_boundaries

end module test_boundaries
