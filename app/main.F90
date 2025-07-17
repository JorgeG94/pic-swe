program main
   use pic, only: pic_print_banner
   use pic_types, only: sp, dp, default_int
   use pic_timers, only: pic_timer_type
   use pic_matrix_printer, only: print_array
   use pic_grid_2d
   use pic_state_2d
   use pic_shallow_water_driver
   use pic_logger, only: global => global_logger, info_level
   implicit none
   real(dp), dimension(:, :), allocatable :: A, B, C
   integer(default_int) :: n, m, k
   real(dp) :: xmin, xmax, ymin, ymax, dx, dy
   real(dp), parameter :: zero = 0.0_dp
   type(grid_2d_type) :: grid

   call pic_print_banner()
   call global%configure(info_level)

   xmin = zero
   ymin = zero
   xmax = 700_dp
   ymax = 200_dp
   dx = 1.0_dp
   block
      type(pic_timer_type) :: my_timer
      real(dp) :: elapsed_time
      type(state_2d_type) :: state
      real(dp) :: t_end
      real(dp) :: cfl
      real(dp), parameter :: h_left = 20.0_dp
      real(dp), parameter :: h_right = 7.0_dp
      real(dp), parameter :: x_dplit = 250.0_dp

      t_end = 5.0_dp
      cfl = 0.45_dp
      call my_timer%start()
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)
      call my_timer%stop()
      elapsed_time = my_timer%get_elapsed_time()
      call global%info("Grid generation tok "//to_string(elapsed_time)//" seconds")
      call global%info("Grid size: nx = "//to_string(grid%nx)//" ny = "//to_string(grid%ny))
      call global%info("Total number of points is "//to_string(grid%nx*grid%ny))

      call state%initialize_state(grid)
      call global%info("Dam break initialized with h_left = "//to_string(h_left)// &
                       ", h_right = "//to_string(h_right)//", x_dplit = "//to_string(x_dplit))
      call initialize_dam_break(state, h_left, h_right, x_dplit)

      call my_timer%start()
      call time_loop(state, t_end, cfl)
      call my_timer%stop()
      elapsed_time = my_timer%get_elapsed_time()

      call global%info("Time loop took "//to_string(elapsed_time)//" seconds")
   end block
end program main
