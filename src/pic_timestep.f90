module pic_timestep
  use pic_types, only: default_int, dp
  use pic_state_2d, only: state_2d_type 
  use local_pic_constants, only: gravity
  implicit none 

  contains 

  function compute_dt(state, cfl) result(dt)
    type(state_2d_type), intent(in) :: state 
    real(dp), intent(in) :: cfl 
    real(dp) :: dt 
    real(dp), parameter :: epsilon = 1.0e-6_dp

    integer(default_int) :: i,j,nx,ny 
    real(dp) :: dx, dy, h, u, v, a, max_speed 

    nx = state%grid%nx 
    ny = state%grid%ny 
    dx = state%grid%dx
    dy = state%grid%dy 

    max_speed = 0.0_dp 

    !do concurrent (j = 1:ny, i = 1:nx)
    do j = 1, ny 
    do i = 1, nx
      h = state%water_height(i,j)

      if (h>epsilon) then 
        u = state%x_momentum(i,j) / h 
        v = state%y_momentum(i,j) / h 
        a = max(abs(u) + sqrt(gravity * h), abs(v) + sqrt(gravity * h))
        max_speed = max(max_speed, a)
      end if 
    end do 
    end do

    if(max_speed > 0.0_dp) then 
      dt = cfl * min(dx,dy) / max_speed 
    else 
      dt = 1.0e-3_dp
    end if
  end function compute_dt

end module pic_timestep
