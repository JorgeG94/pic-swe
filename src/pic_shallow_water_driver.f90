module pic_shallow_water_driver
   use pic_types, only: default_int, dp
   use pic_state_2d, only: state_2d_type
   use pic_flux_2d, only: compute_rusanov_flux_x, compute_rusanov_flux_y
   use pic_update_2d, only: update_state, enforce_min_height
   use pic_timestep, only: compute_dt
   use pic_timers
   use pic_logger, only: global=> global_logger
   use pic_string_utils, only: to_string

   implicit none


contains

  subroutine check_mass_conservation(state, initial_mass, step)
      class(state_2d_type), intent(in) :: state
      real(dp), intent(in) :: initial_mass
      integer, intent(in) :: step
      real(dp) :: current_mass
      current_mass = sum(state%water_height) * state%grid%dx * state%grid%dy
      call global%verbose("Step " // to_string(step) // " Mass conservation check: " // &
         "Initial mass: " // to_string(initial_mass) // &
         ", Current mass: " // to_string(current_mass) // &
         ", Δmass: " // to_string(current_mass - initial_mass))

   end subroutine check_mass_conservation

   subroutine write_water_height_to_csv(state, step)
      use, intrinsic :: iso_fortran_env, only: dp => real64
      class(state_2d_type), intent(in) :: state
      integer, intent(in) :: step
      character(len=100) :: filename
      integer :: i, j
      filename = "height_step_" // to_string(step) // ".csv"

      open(unit=100, file=filename, status='replace')
      do j = 1, state%grid%ny
         do i = 1, state%grid%nx
            write(100, '(F12.5)', advance='no') state%water_height(i,j)
            if (i < state%grid%nx) write(100, '(A)', advance='no') ","
         end do
         write(100, *)  ! New line
      end do

      close(100)
   end subroutine write_water_height_to_csv


   subroutine apply_reflective_boundaries(state)
      type(state_2d_type), intent(inout) :: state
      integer :: nx, ny
      nx = state%grid%nx
      ny = state%grid%ny

      ! Reflective in X direction
      state%water_height(1,:)     = state%water_height(2,:)
      state%water_height(nx,:)    = state%water_height(nx-1,:)
      state%x_momentum(1,:)       = -state%x_momentum(2,:)
      state%x_momentum(nx,:)      = -state%x_momentum(nx-1,:)
      state%y_momentum(1,:)       = state%y_momentum(2,:)
      state%y_momentum(nx,:)      = state%y_momentum(nx-1,:)

      ! Reflective in Y direction
      state%water_height(:,1)     = state%water_height(:,2)
      state%water_height(:,ny)    = state%water_height(:,ny-1)
      state%x_momentum(:,1)       = state%x_momentum(:,2)
      state%x_momentum(:,ny)      = state%x_momentum(:,ny-1)
      state%y_momentum(:,1)       = -state%y_momentum(:,2)
      state%y_momentum(:,ny)      = -state%y_momentum(:,ny-1)

      ! Corner values (to avoid double overwrite)
      state%water_height(1,1)     = state%water_height(2,2)
      state%x_momentum(1,1)       = -state%x_momentum(2,2)
      state%y_momentum(1,1)       = -state%y_momentum(2,2)

      state%water_height(1,ny)    = state%water_height(2,ny-1)
      state%x_momentum(1,ny)      = -state%x_momentum(2,ny-1)
      state%y_momentum(1,ny)      = -state%y_momentum(2,ny-1)

      state%water_height(nx,1)    = state%water_height(nx-1,2)
      state%x_momentum(nx,1)      = -state%x_momentum(nx-1,2)
      state%y_momentum(nx,1)      = -state%y_momentum(nx-1,2)

      state%water_height(nx,ny)   = state%water_height(nx-1,ny-1)
      state%x_momentum(nx,ny)     = -state%x_momentum(nx-1,ny-1)
      state%y_momentum(nx,ny)     = -state%y_momentum(nx-1,ny-1)

   end subroutine apply_reflective_boundaries


   subroutine time_loop(state, t_end, cfl)

      type(state_2d_type), intent(inout) :: state
      real(dp), intent(in) :: t_end, cfl
      integer(default_int) :: nx, ny

      real(dp) :: dt, t
      real(dp), allocatable :: flux_x_h(:,:), flux_x_hu(:,:), flux_x_hv(:,:)
      real(dp), allocatable :: flux_y_h(:,:), flux_y_hu(:,:), flux_y_hv(:,:)
      integer(default_int) :: step, print_interval

      t = 0.0_dp
      step = 0
      print_interval = 10

      nx = state%grid%nx
      ny = state%grid%ny

      allocate(flux_x_h(nx+1, ny), flux_x_hu(nx+1, ny), flux_x_hv(nx+1, ny))
      allocate(flux_y_h(nx, ny+1), flux_y_hu(nx, ny+1), flux_y_hv(nx, ny+1))
      evolve_loop: block
         type(pic_timer_type) :: my_timer
         real(dp) :: elapsed_time
         real(dp), parameter :: h_min = 1.0e-5_dp
         real(dp) :: before_mass, after_mass, initial_mass, final_mass
         initial_mass = sum(state%water_height) * state%grid%dx * state%grid%dy
         do while (t < t_end)
            if (mod(step, print_interval) == 0) then
               call my_timer%start()
            endif
            dt = compute_dt(state, cfl)

            if(t + dt > t_end) dt = t_end - t

            call apply_reflective_boundaries(state)

            ! Compute fluxes
            call compute_rusanov_flux_x(state, flux_x_h, flux_x_hu, flux_x_hv)
            call compute_rusanov_flux_y(state, flux_y_h, flux_y_hu, flux_y_hv)

            ! Update state
            before_mass= sum(state%water_height) * state%grid%dx  * state%grid%dy
            call update_state(state, flux_x_h, flux_x_hu, flux_x_hv, &
               flux_y_h, flux_y_hu, flux_y_hv, dt)
            call enforce_min_height(state, h_min)
            after_mass = sum(state%water_height) * state%grid%dx  * state%grid%dy


            ! Advance time
            t = t + dt

            step = step + 1

            if (mod(step, print_interval) == 0) then
               call my_timer%stop()
               elapsed_time = my_timer%get_elapsed_time()
               call check_mass_conservation(state, initial_mass, step)
               printing: block

                  real(dp) :: total_mass
                  real(dp) :: total_mom_x, total_mom_y
                  real(dp) :: net_flux
                  net_flux = sum(flux_x_h(nx+1,:)) - sum(flux_x_h(1,:)) + &
                     sum(flux_y_h(:,ny+1)) - sum(flux_y_h(:,1))
                  total_mass = sum(state%water_height) * state%grid%dx * state%grid%dy
                  total_mom_x = sum(state%x_momentum) * state%grid%dx * state%grid%dy
                  total_mom_y = sum(state%y_momentum) * state%grid%dx * state%grid%dy

                  call global%info("Step " // to_string(step) // " time " // to_string(t) // &
                     " dt " // to_string(dt) // " time per steps " // to_string(elapsed_time) // &
                     " mass " // to_string(total_mass) // &
                     " net flux " // to_string(net_flux) //  " Δmass " // to_string(after_mass - before_mass))

               end block printing

               call write_water_height_to_csv(state, step)
            end if

            final_mass = sum(state%water_height) * state%grid%dx * state%grid%dy


         end do

      call global%info("Final mass: " // to_string(final_mass) // &
         ", Initial mass: " // to_string(initial_mass) // &
         ", Δmass: " // to_string(final_mass - initial_mass))
      end block evolve_loop

      deallocate(flux_x_h, flux_x_hu, flux_x_hv)
      deallocate(flux_y_h, flux_y_hu, flux_y_hv)
   end subroutine time_loop

end module pic_shallow_water_driver
