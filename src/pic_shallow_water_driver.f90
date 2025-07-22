module pic_shallow_water_driver
   use pic_types, only: default_int, dp, sp
   use pic_gpu_utils, only: dasum
   use pic_state_2d, only: state_2d_type
   use pic_flux_2d, only: compute_rusanov_fluxes_xy, flux_type
   use pic_boundaries, only: apply_reflective_boundaries
   use pic_update_2d, only: enforce_min_height, update_state_block
   use pic_timestep, only: compute_dt
   use pic_timers
   use pic_logger, only: global => global_logger
   use pic_string_utils, only: to_string

   implicit none

contains

   function pad(s, width) result(padded)
      character(len=*), intent(in) :: s
      integer, intent(in) :: width
      character(len=:), allocatable :: padded
      integer :: len_s

      len_s = len_trim(s)
      if (len_s >= width) then
         padded = s(1:width)
      else
         padded = repeat(" ", width - len_s)//s
      end if
   end function pad

   pure function round_dp(x, ndigits) result(r)
      real(dp), intent(in) :: x
      integer, intent(in) :: ndigits
      real(dp) :: r
      r = real(nint(x*10.0_dp**ndigits))/10.0_dp**ndigits
   end function round_dp

   subroutine check_mass_conservation(state, initial_mass, step)
      class(state_2d_type), intent(in) :: state
      real(dp), intent(in) :: initial_mass
      integer, intent(in) :: step
      real(dp) :: current_mass
      current_mass = sum(state%water_height)*state%grid%dx*state%grid%dy
      call global%verbose("Step "//to_string(step)//" Mass conservation check: "// &
                          "Initial mass: "//to_string(initial_mass)// &
                          ", Current mass: "//to_string(current_mass)// &
                          ", Δmass: "//to_string(current_mass - initial_mass))

   end subroutine check_mass_conservation

   subroutine write_water_height_to_csv(state, step)
      use, intrinsic :: iso_fortran_env, only: dp => real64
      class(state_2d_type), intent(in) :: state
      integer, intent(in) :: step
      character(len=100) :: filename
      integer :: i, j
      filename = "height_step_"//to_string(step)//".csv"
      open (unit=100, file=filename, status='replace')
      do j = 1, state%grid%ny
         do i = 1, state%grid%nx
            write (100, '(F12.5)', advance='no') state%water_height(i, j)
            if (i < state%grid%nx) write (100, '(A)', advance='no') ","
         end do
         write (100, *)  ! New line
      end do

      close (100)
   end subroutine write_water_height_to_csv

   subroutine time_loop(state, t_end, cfl)

      type(state_2d_type), intent(inout) :: state
      real(dp), intent(in) :: t_end, cfl
      type(flux_type) :: flux_x, flux_y 
      integer(default_int) :: nx, ny

      real(dp) :: dt, t
      real(dp), allocatable :: flux_x_h(:, :), flux_x_hu(:, :), flux_x_hv(:, :)
      real(dp), allocatable :: flux_y_h(:, :), flux_y_hu(:, :), flux_y_hv(:, :)
      integer(default_int) :: step, print_interval

      t = 0.0_dp
      step = 0
      print_interval = 10

      nx = state%grid%nx
      ny = state%grid%ny

      call flux_x%allocate_fluxes(nx+1, ny) 
      call flux_y%allocate_fluxes(nx, ny+1) 

      evolve_loop: block
         type(pic_timer_type) :: my_timer
         type(pic_timer_type) :: inner_timer
         real(dp) :: elapsed_time
         real(dp), parameter :: h_min = 1.0e-5_dp
         real(dp) :: before_mass, after_mass, initial_mass, final_mass
         real(dp) :: total_mass
         real(dp) :: total_mom_x, total_mom_y
         real(dp) :: net_flux
         integer :: i,j 
         net_flux = 0.0_dp
         before_mass = 0.0_dp
         after_mass = 0.0_dp
         initial_mass = sum(state%water_height)*state%grid%dx*state%grid%dy
         call global%info( &
            pad("Step", 8)//pad("Time", 12)//pad("dt", 12)// &
            pad("Time/Step", 14)//pad("Mass", 12)//pad("ΔMass", 12))

         !$omp target enter data map(alloc: flux_x, flux_y, flux_x%flux_h, flux_x%flux_hu, flux_x%flux_hv, flux_y%flux_h, flux_y%flux_hu,flux_y%flux_hv)

         !$omp target data map(tofrom: state) map(tofrom: state%water_height, state%x_momentum, state%y_momentum)
         call flux_x%set_fluxes(0.0_dp, .true.)
         call flux_y%set_fluxes(0.0_dp, .true.)
         do while (t < t_end)
            if (mod(step, print_interval) == 0) then
               call my_timer%start()
            end if
            dt = compute_dt(state, cfl)

            if (t + dt > t_end) dt = t_end - t

            call apply_reflective_boundaries(state)

            ! Compute fluxes
            call compute_rusanov_fluxes_xy(state, flux_x, flux_y)

            before_mass = dasum(state%water_height)*state%grid%dx*state%grid%dy
            ! Update state
            call update_state_block(state, flux_x, flux_y, dt)
            call enforce_min_height(state, h_min)

            after_mass = dasum(state%water_height)*state%grid%dx*state%grid%dy

            ! Advance time
            t = t + dt
            step = step + 1

            if (mod(step, print_interval) == 0) then
               call my_timer%stop()
               elapsed_time = my_timer%get_elapsed_time()
               !call check_mass_conservation(state, initial_mass, step)
               !printing: block
                  
                  total_mass = dasum(state%water_height)*state%grid%dx*state%grid%dy 
                  total_mom_x = dasum(state%x_momentum)*state%grid%dx*state%grid%dy
                  total_mom_y = dasum(state%y_momentum)*state%grid%dx*state%grid%dy

                  call global%info( &
                     pad(to_string(step), 8)//" "//pad(to_string(round_dp(t, 4)), 12)//" "// &
                     pad(to_string(round_dp(dt, 4)), 12)//" "//pad(to_string(round_dp(elapsed_time, 4)), 14)//" "// &
                     pad(to_string(total_mass), 12)//" "// &
                     pad(to_string(round_dp((after_mass - before_mass), 4)), 12))

              ! end block printing

               !call write_water_height_to_csv(state, step)
            end if

         end do
         !$omp end target data
         !$omp target exit data map(release: flux_x, flux_y, flux_x%flux_h, flux_x%flux_hu, flux_x%flux_hv, flux_y%flux_h, flux_y%flux_hu,flux_y%flux_hv)
          final_mass = sum(state%water_height)*state%grid%dx*state%grid%dy

         call global%info("Final mass: "//to_string(final_mass)// &
                          ", Initial mass: "//to_string(initial_mass)// &
                          ", Δmass: "//to_string(final_mass - initial_mass)// &
                          " Lost "//to_string(100*(1 - (final_mass/initial_mass)))//" % of the initial mass")
      end block evolve_loop

      call flux_x%deallocate_fluxes()
      call flux_y%deallocate_fluxes()
      ! deallocate (flux_x_h, flux_x_hu, flux_x_hv)
      ! deallocate (flux_y_h, flux_y_hu, flux_y_hv)
   end subroutine time_loop

end module pic_shallow_water_driver
