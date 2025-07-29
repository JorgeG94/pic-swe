module test_timestep
   use pic_test_helpers, only: is_equal
   use pic_string_utils, only: to_string
   use pic_types, only: dp
   use pic_swe_timestep, only: compute_dt
   use pic_swe_grid_2d, only: grid_2d_type, init_grid, generate_2d_grids
   use pic_swe_state_2d, only: state_2d_type, initialize_state, initialize_dam_break
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   implicit none
   private
   public :: collect_tests_timestep

contains

   subroutine collect_tests_timestep(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2

      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_compute_dt_zero", test_compute_dt_zero)
      testsuite(2) = new_unittest("test_compute_dt_positive", test_compute_dt_positive)

   end subroutine collect_tests_timestep

   subroutine test_compute_dt_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx
      type(state_2d_type) :: state
      real(dp) :: cfl, dt

      cfl = 0.5_dp

      xmin = 0.0_dp
      xmax = 2.0_dp
      ymin = 0.0_dp
      ymax = 1.0_dp
      dx = 1.0_dp
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)

      ! Initialize a dummy state
      call initialize_state(state, grid)

      ! Compute the time step
      dt = compute_dt(state, cfl)
      call check(error, is_equal(dt, 1.0e-3_dp), "Time step should be 1.0e-3_dp for zero water height")

   end subroutine test_compute_dt_zero

   subroutine test_compute_dt_positive(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx
      type(state_2d_type) :: state
      real(dp), parameter :: expected_dt = 0.11288091024643272_dp
      real(dp) :: cfl, dt

      cfl = 0.5_dp

      xmin = 0.0_dp
      xmax = 2.0_dp
      ymin = 0.0_dp
      ymax = 1.0_dp
      dx = 1.0_dp
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)

      ! Initialize a dummy state
      call initialize_state(state, grid)

      call initialize_dam_break(state, 2.0_dp, 0.5_dp, 1.0_dp)

      ! Compute the time step
      dt = compute_dt(state, cfl)
      call check(error, is_equal(dt, expected_dt), "Time step should be "//to_string(expected_dt))

   end subroutine test_compute_dt_positive

end module test_timestep
