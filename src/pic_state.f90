module pic_state_2d
use pic_types, only: default_int, dp, sp
use pic_grid_2d
implicit none 

type :: state_2d_type 

  type(grid_2d_type) :: grid 

  real(dp), allocatable :: water_height(:,:)
  real(dp), allocatable :: x_momentum(:,:)
  real(dp), allocatable :: y_momentum(:,:)
  real(dp), allocatable :: ground_elevation(:,:)

  logical :: is_initialized = .false.

  contains 
  procedure :: initialize_state 

  end type state_2d_type


  contains 

  subroutine initialize_state(state, grid)
    class(state_2d_type), intent(inout) :: state 
    type(grid_2d_type), intent(in) :: grid 

    state%grid = grid 

    allocate(state%water_height(grid%nx, grid%ny))
    allocate(state%x_momentum(grid%nx, grid%ny))
    allocate(state%y_momentum(grid%nx, grid%ny))
    allocate(state%ground_elevation(grid%nx, grid%ny))

    block 
    integer(default_int) :: i,j 

    !do concurrent (j=1:grid%ny, i = 1:grid%nx)
  do j = 1, grid%ny
  do i = 1, grid%nx
      state%water_height(i,j) = 0.0_dp
      state%x_momentum(i,j) = 0.0_dp
      state%y_momentum(i,j) = 0.0_dp
      state%ground_elevation(i,j) = 0.0_dp
    end do
    end do 
    end block

    state%is_initialized = .true.

    end subroutine initialize_state 

subroutine initialize_dam_break(state, h_left, h_right, x_dplit)
  type(state_2d_type), intent(inout) :: state
  real(dp), intent(in) :: h_left, h_right, x_dplit
  integer(default_int) :: i, j
  type(grid_2d_type) :: grid

  if (.not. state%is_initialized) error stop "State must be initialized first"

  grid = state%grid

  !do concurrent (j = 1:grid%ny, i = 1:grid%nx)
  do j = 1, grid%ny
  do i = 1, grid%nx
    if (grid%x(i) < x_dplit) then
      state%water_height(i,j) = h_left
    else
      state%water_height(i,j) = h_right
    end if
    state%x_momentum(i,j) = 0.0_dp
    state%y_momentum(i,j) = 0.0_dp
  end do
  end do
end subroutine


  end module pic_state_2d
