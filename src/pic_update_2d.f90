module pic_update_2d
  use pic_types, only: default_int, dp
  use pic_state_2d, only: state_2d_type 
  use local_pic_constants, only: epsilon
  implicit none 

  contains 

  subroutine update_state(state, flux_x_h, flux_x_hu, flux_x_hv, &
                          flux_y_h, flux_y_hu, flux_y_hv, dt)
    class(state_2d_type), intent(inout) :: state
    real(dp), intent(in) :: flux_x_h(:,:), flux_x_hu(:,:), flux_x_hv(:,:)
    real(dp), intent(in) :: flux_y_h(:,:), flux_y_hu(:,:), flux_y_hv(:,:)
    real(dp), intent(in) :: dt

    integer :: i, j, nx, ny, ii, jj, i_loc, j_loc 
    integer, parameter :: bx = 32, by = 32
    real(dp) :: height(bx, by), x_mom(bx, by), y_mom(bx,by)
    real(dp) :: dx, dy

    nx = state%grid%nx
    ny = state%grid%ny
    dx = state%grid%dx
    dy = state%grid%dy

!    do concurrent (j = 1:ny, i = 1:nx)
    !$omp parallel do collapse(2) 
    do jj = 1, ny-1, by
  do ii = 1, nx-1, bx

    ! Compute flux updates into local buffer
    do j_loc = 1, min(by, ny-1 - jj + 1)
      j = jj + j_loc - 1
      do i_loc = 1, min(bx, nx-1 - ii + 1)
        i = ii + i_loc - 1

        height(i_loc,j_loc) = - dt/dx * (flux_x_h(i+1,j) - flux_x_h(i,j)) &
                              - dt/dy * (flux_y_h(i,j+1) - flux_y_h(i,j))

        x_mom(i_loc,j_loc) = - dt/dx * (flux_x_hu(i+1,j) - flux_x_hu(i,j)) &
                             - dt/dy * (flux_y_hu(i,j+1) - flux_y_hu(i,j))

        y_mom(i_loc,j_loc) = - dt/dx * (flux_x_hv(i+1,j) - flux_x_hv(i,j)) &
                             - dt/dy * (flux_y_hv(i,j+1) - flux_y_hv(i,j))
      end do
    end do

    ! Write once per cell from local buffers
    do j_loc = 1, min(by, ny-1 - jj + 1)
      j = jj + j_loc - 1
      do i_loc = 1, min(bx, nx-1 - ii + 1)
        i = ii + i_loc - 1

        state%water_height(i,j) = state%water_height(i,j) + height(i_loc,j_loc)
        state%x_momentum(i,j)   = state%x_momentum(i,j)   + x_mom(i_loc,j_loc)
        state%y_momentum(i,j)   = state%y_momentum(i,j)   + y_mom(i_loc,j_loc)
      end do
    end do

  end do
end do
    !$omp end parallel do
  end subroutine update_state

subroutine enforce_min_height(state, h_min)
  class(state_2d_type), intent(inout) :: state
  real(dp), intent(in) :: h_min
  integer :: i, j, nx, ny

  nx = state%grid%nx
  ny = state%grid%ny

  do j = 1, ny
    do i = 1, nx
      if (state%water_height(i,j) < h_min) then
        state%water_height(i,j) = 0.0_dp
        state%x_momentum(i,j) = 0.0_dp
        state%y_momentum(i,j) = 0.0_dp
      end if
    end do
  end do
end subroutine enforce_min_height


end module pic_update_2d
