module test_fluxes
   use pic_test_helpers, only: is_equal
   use pic_matrix_printer, only: print_array
   use pic_string_utils, only: to_string
   use pic_types, only: dp
   use pic_swe_flux_2d, only: compute_rusanov_fluxes_xy, flux_type
   use pic_swe_grid_2d, only: grid_2d_type, init_grid, generate_2d_grids
   use pic_swe_state_2d, only: state_2d_type, initialize_state, initialize_dam_break
   use pic_swe_boundaries, only: apply_reflective_boundaries
   use pic_swe_timestep, only: compute_dt
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   implicit none
   private
   public :: collect_tests_fluxes

contains

   subroutine collect_tests_fluxes(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer, parameter :: ntests = 2

      allocate (testsuite(ntests))
      testsuite(1) = new_unittest("test_compute_rusanov_fluxes", test_compute_rusanov_fluxes)
      testsuite(2) = new_unittest("test_compute_rusanov_fluxes", test_compute_rusanov_fluxes_nonzero)

   end subroutine collect_tests_fluxes

   subroutine test_compute_rusanov_fluxes(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx
      type(state_2d_type) :: state
      real(dp), parameter :: expected_dt = 0.11288091024643272_dp
      real(dp) :: cfl, dt
      type(flux_type) :: flux_x, flux_y

      real(dp), parameter :: flux_x_h_ref(4, 2) = reshape([ &
                            0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                          ], [4, 2])
      real(dp), parameter :: flux_x_hu_ref(4, 2) = reshape([ &
                            0.000000_dp, 1.226250_dp, 1.226250_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [4, 2])
      real(dp), parameter :: flux_x_hv_ref(4, 2) = reshape([ &
                            0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [4, 2])
      real(dp), parameter :: flux_y_h_ref(3, 3) = reshape([ &
              0.000000_dp, 0.000000_dp, 0.000000_dp, -0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                          ], [3, 3])
      real(dp), parameter :: flux_y_hu_ref(3, 3) = reshape([ &
               0.000000_dp, 0.000000_dp, 0.000000_dp, 1.226250_dp, 1.226250_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [3, 3])
      real(dp), parameter :: flux_y_hv_ref(3, 3) = reshape([ &
               0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [3, 3])
      integer :: nx, ny

      cfl = 0.5_dp

      xmin = 0.0_dp
      xmax = 2.0_dp
      ymin = 0.0_dp
      ymax = 1.0_dp
      dx = 1.0_dp
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)

      nx = grid%nx
      ny = grid%ny

      ! Initialize a dummy state
      call initialize_state(state, grid)

      call initialize_dam_break(state, 2.0_dp, 0.5_dp, 1.0_dp)

      ! Compute the time step
      dt = compute_dt(state, cfl)
      !dt = 2.0_dp
      call check(error, is_equal(dt, expected_dt), "Time step should be "//to_string(expected_dt))
      call apply_reflective_boundaries(state)


      call flux_x%allocate_fluxes(nx + 1, ny)
      call flux_y%allocate_fluxes(nx, ny + 1)
      call flux_x%set_fluxes(0.0_dp)
      call flux_y%set_fluxes(0.0_dp)

      ! flux_x_h = 0.0_dp
      ! flux_x_hu = 0.0_dp
      ! flux_x_hv = 0.0_dp
      ! flux_y_h = 0.0_dp
      ! flux_y_hu = 0.0_dp
      ! flux_y_hv = 0.0_dp

      call compute_rusanov_fluxes_xy(state, flux_x, flux_y)
      call check(error, all(is_equal(flux_x%flux_h, flux_x_h_ref)), "flux_x_h should match reference")
      call check(error, all(is_equal(flux_x%flux_hu, flux_x_hu_ref)), "flux_x_hu should match reference")
      call check(error, all(is_equal(flux_x%flux_hv, flux_x_hv_ref)), "flux_x_hv should match reference")
      call check(error, all(is_equal(flux_y%flux_h, flux_y_h_ref)), "flux_y_h should match reference")
      call check(error, all(is_equal(flux_y%flux_hu, flux_y_hu_ref)), "flux_y_hu should match reference")
      call check(error, all(is_equal(flux_y%flux_hv, flux_y_hv_ref)), "flux_y_hv should match reference")

      call flux_x%deallocate_fluxes()
      call flux_y%deallocate_fluxes()

   end subroutine test_compute_rusanov_fluxes

   subroutine test_compute_rusanov_fluxes_nonzero(error)
      type(error_type), allocatable, intent(out) :: error
      type(grid_2d_type) :: grid
      real(dp) :: xmin, xmax, ymin, ymax, dx
      type(state_2d_type) :: state
      real(dp), parameter :: expected_dt = 0.11288091024643272_dp
      real(dp) :: cfl, dt
      type(flux_type) :: flux_x, flux_y
      real(dp), parameter :: flux_x_h_ref(4, 2) = reshape([ &
                            0.000000_dp, 5.000000_dp, 5.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                          ], [4, 2])
      real(dp), parameter :: flux_x_hu_ref(4, 2) = reshape([ &
                          0.000000_dp, 51.226250_dp, 51.226250_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [4, 2])
      real(dp), parameter :: flux_x_hv_ref(4, 2) = reshape([ &
                          0.000000_dp, 20.000000_dp, 20.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [4, 2])
      real(dp), parameter :: flux_y_h_ref(3, 3) = reshape([ &
               0.000000_dp, 0.000000_dp, 0.000000_dp, 5.000000_dp, 5.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                          ], [3, 3])
      real(dp), parameter :: flux_y_hu_ref(3, 3) = reshape([ &
             0.000000_dp, 0.000000_dp, 0.000000_dp, 51.226250_dp, 51.226250_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [3, 3])
      real(dp), parameter :: flux_y_hv_ref(3, 3) = reshape([ &
             0.000000_dp, 0.000000_dp, 0.000000_dp, 20.000000_dp, 20.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp, 0.000000_dp &
                                                           ], [3, 3])

      integer :: nx, ny

      cfl = 0.5_dp

      xmin = 0.0_dp
      xmax = 2.0_dp
      ymin = 0.0_dp
      ymax = 1.0_dp
      dx = 1.0_dp
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)

      nx = grid%nx
      ny = grid%ny

      ! Initialize a dummy state
      call initialize_state(state, grid)

      call initialize_dam_break(state, 2.0_dp, 0.5_dp, 1.0_dp)

      ! Compute the time step
      dt = compute_dt(state, cfl)
      !dt = 2.0_dp
      call check(error, is_equal(dt, expected_dt), "Time step should be "//to_string(expected_dt))
      call apply_reflective_boundaries(state)

      call flux_x%allocate_fluxes(nx + 1, ny)
      call flux_y%allocate_fluxes(nx, ny + 1)
      call flux_x%set_fluxes(0.0_dp)
      call flux_y%set_fluxes(0.0_dp)

      block
         integer :: i, j
         do j = 1, ny
            do i = 1, nx
               state%x_momentum(i, j) = 5.0_dp
               state%y_momentum(i, j) = 2.0_dp
            end do
         end do
      end block

      call compute_rusanov_fluxes_xy(state, flux_x, flux_y)

      call check(error, all(is_equal(flux_x%flux_h, flux_x_h_ref)), "flux_x_h should match reference")
      call check(error, all(is_equal(flux_x%flux_hu, flux_x_hu_ref)), "flux_x_hu should match reference")
      call check(error, all(is_equal(flux_x%flux_hv, flux_x_hv_ref)), "flux_x_hv should match reference")
      call check(error, all(is_equal(flux_y%flux_h, flux_y_h_ref)), "flux_y_h should match reference")
      call check(error, all(is_equal(flux_y%flux_hu, flux_y_hu_ref)), "flux_y_hu should match reference")
      call check(error, all(is_equal(flux_y%flux_hv, flux_y_hv_ref)), "flux_y_hv should match reference")

      call flux_x%deallocate_fluxes()
      call flux_y%deallocate_fluxes()

   end subroutine test_compute_rusanov_fluxes_nonzero

end module test_fluxes
