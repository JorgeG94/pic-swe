program main
   use pic, only: pic_print_banner
   use pic_types, only: dp, default_int
   use pic_timers, only: pic_timer_type
   use pic_matrix_printer, only: print_array
   use pic_grid_2d
   use pic_state_2d
   use pic_shallow_water_driver
   implicit none
   real(dp), dimension(:, :), allocatable :: A, B, C
   integer(default_int) :: n, m, k
   real(dp) :: xmin, xmax, ymin, ymax, dx, dy
   real(dp), parameter :: zero = 0.0_dp
   type(grid_2d_type) :: grid

   call pic_print_banner()

   xmin = zero
   ymin = zero
   xmax = 500_dp
   ymax = 200_dp
   dx = 1.0_dp
   block
      type(pic_timer_type) :: my_timer
      real(dp) :: elapsed_time
      type(state_2d_type) :: state
      real(dp) :: t_end
      real(dp) :: cfl

      t_end = 50.0_dp
      cfl = 0.3_dp
      call my_timer%start()
      call init_grid(grid, xmin, xmax, ymin, ymax, dx)
      call generate_2d_grids(grid)
      call my_timer%stop()
      elapsed_time = my_timer%get_elapsed_time()
      print *, "grid generation took", elapsed_time, " seconds"

      print *, 'Grid dimensions: nx =', grid%nx, ' ny =', grid%ny
      print *, "Total number of points is ", grid%nx*grid%ny
      call state%initialize_state(grid)
      call initialize_dam_break(state, h_left=20.0_dp, h_right=7.0_dp, x_split=250.0_dp)

      call my_timer%start()
      call time_loop(state, t_end, cfl)
      call my_timer%stop()
      elapsed_time = my_timer%get_elapsed_time()

      print *, "done took ", elapsed_time, " seconds"
   end block
end program main
