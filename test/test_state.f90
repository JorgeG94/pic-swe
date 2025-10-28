module test_state
   use pic_test_helpers, only: is_equal
   use pic_types, only: dp
   use pic_io, only: to_char
   use pic_swe_state_2d, only: state_2d_type, initialize_state, initialize_dam_break
   use pic_swe_grid_2d, only: grid_2d_type, init_grid
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   implicit none
   private

   public :: collect_tests_state

contains

   subroutine collect_tests_state(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2

      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_initialize_state", test_initialize_state)
      testsuite(2) = new_unittest("test_initialize_dam_break", test_initialize_dam_break)

   end subroutine collect_tests_state

   subroutine test_initialize_state(error)
      type(error_type), allocatable, intent(out) :: error
      type(state_2d_type) :: state
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx

      xmin = 0.0_dp
      xmax = 10.0_dp
      ymin = 0.0_dp
      ymax = 5.0_dp
      dx = 1.0_dp

      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      ! we've united tested the initialization of the grid, so we can use it here safely
      call initialize_state(state, grid)

      call check(error, state%is_initialized)
      call check(error, is_equal(state%grid%xmin, xmin))
      call check(error, is_equal(state%grid%xmax, xmax))
      call check(error, is_equal(state%grid%ymin, ymin))
      call check(error, is_equal(state%grid%ymax, ymax))
      call check(error, allocated(state%water_height))
      call check(error, allocated(state%x_momentum))
      call check(error, allocated(state%y_momentum))
      call check(error, allocated(state%ground_elevation))

      sum_block: block
         real(dp) :: sum_of_elements

         sum_of_elements = sum(state%water_height)
         call check(error, is_equal(sum_of_elements, 0.0_dp), "Sum of water height should be zero")

         sum_of_elements = sum(state%x_momentum)
         call check(error, is_equal(sum_of_elements, 0.0_dp), "Sum of x momentum should be zero")

         sum_of_elements = sum(state%y_momentum)
         call check(error, is_equal(sum_of_elements, 0.0_dp), "Sum of y momentum should be zero")

         sum_of_elements = sum(state%ground_elevation)
         call check(error, is_equal(sum_of_elements, 0.0_dp), "Sum of ground elevation should be zero")

      end block sum_block

      if (allocated(error)) return

   end subroutine test_initialize_state

   subroutine test_initialize_dam_break(error)
      type(error_type), allocatable, intent(out) :: error
      type(state_2d_type) :: state
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx
      real(dp), parameter :: h_left = 2.0_dp
      real(dp), parameter :: h_right = 0.5_dp
      real(dp), parameter :: x_split = 2.5_dp
      real(dp) :: sum_height_left, sum_height_right
      real(dp) :: correct_height_left = 50.0_dp
      real(dp) :: correct_height_right = 13.0_dp
      xmin = 0.0_dp
      xmax = 5.0_dp
      ymin = 0.0_dp
      ymax = 1.0_dp
      dx = 0.1_dp

      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call initialize_state(state, grid)

      call initialize_dam_break(state, h_left, h_right, x_split)

      height_sum: block
         integer :: i, j

         do j = 1, grid%ny
            sum_height_left = 0.0_dp
            sum_height_right = 0.0_dp
            do i = 1, grid%nx
               if (grid%x(i) < x_split) then
                  sum_height_left = sum_height_left + state%water_height(i, j)
               else
                  sum_height_right = sum_height_right + state%water_height(i, j)
               end if
            end do
         end do

      call check(error, is_equal(sum_height_left, correct_height_left), "Sum of water height left of split should be "//to_char(correct_height_left))
      call check(error, is_equal(sum_height_right, correct_height_right), "Sum of water height right of split should be "//to_char(correct_height_right))
      end block height_sum

   end subroutine test_initialize_dam_break

end module test_state
