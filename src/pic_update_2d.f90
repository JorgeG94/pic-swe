module pic_swe_update_2d
   use pic_types, only: default_int, dp, sp
   use pic_swe_flux_2d, only: flux_type
   use pic_swe_state_2d, only: state_2d_type
   use pic_swe_constants, only: epsilon
   implicit none

contains


    subroutine update_state_block(state, flux_x, flux_y, dt)
      class(state_2d_type), intent(inout) :: state
      type(flux_type), intent(inout) :: flux_x, flux_y
      real(dp), intent(in) :: dt

      integer :: i, j, nx, ny, ii, jj, i_loc, j_loc
      integer, parameter :: bx = 32, by = 32
      real(dp) :: height(bx, by), x_mom(bx, by), y_mom(bx, by)
      real(dp) :: dx, dy
      real(dp) :: dh, dhu, dhv

      nx = state%grid%nx
      ny = state%grid%ny
      dx = state%grid%dx
      dy = state%grid%dy

!!$omp parallel do private(height, x_mom, y_mom, i, j, i_loc, j_loc)
!$omp target teams loop collapse(2) bind(teams) private(dh, dhu, dhv, i, j) &
!$omp num_teams(1024) thread_limit(32) &
!$omp map(to:    flux_x, flux_x%flux_h,  flux_x%flux_hu,  flux_x%flux_hv, &
!$omp            flux_y, flux_y%flux_h,  flux_y%flux_hu,  flux_y%flux_hv) &
!$omp map(tofrom: state, state%water_height, state%x_momentum, state%y_momentum)
do j = 1, ny
  do i = 1, nx
    dh  = -dt/dx * (flux_x%flux_h (i+1, j) - flux_x%flux_h (i, j)) &
          -dt/dy * (flux_y%flux_h (i, j+1) - flux_y%flux_h (i, j))

    dhu = -dt/dx * (flux_x%flux_hu(i+1, j) - flux_x%flux_hu(i, j)) &
          -dt/dy * (flux_y%flux_hu(i, j+1) - flux_y%flux_hu(i, j))

    dhv = -dt/dx * (flux_x%flux_hv(i+1, j) - flux_x%flux_hv(i, j)) &
          -dt/dy * (flux_y%flux_hv(i, j+1) - flux_y%flux_hv(i, j))

    state%water_height(i, j) = state%water_height(i, j) + dh
    state%x_momentum  (i, j) = state%x_momentum  (i, j) + dhu
    state%y_momentum  (i, j) = state%y_momentum  (i, j) + dhv
  end do
end do
!$omp end target teams loop

!      !$omp end parallel do
   end subroutine update_state_block

   subroutine enforce_min_height(state, h_min)
      class(state_2d_type), intent(inout) :: state
      real(dp), intent(in) :: h_min
      integer :: i, j, nx, ny

      nx = state%grid%nx
      ny = state%grid%ny
      !$omp target teams loop collapse(2) private(i,j) shared(h_min) &
      !$omp map(tofrom: state, state%water_height, state%x_momentum, state%y_momentum)
      do j = 1, ny
         do i = 1, nx
            if (state%water_height(i, j) < h_min) then
               state%water_height(i, j) = 0.0_dp
               state%x_momentum(i, j) = 0.0_dp
               state%y_momentum(i, j) = 0.0_dp
            end if
         end do
      end do
      !$omp end target teams loop
   end subroutine enforce_min_height

end module pic_swe_update_2d
