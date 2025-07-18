module pic_boundaries
   use pic_types, only: default_int, dp
   use pic_state_2d, only: state_2d_type
   implicit none

   private 
   public :: apply_reflective_boundaries

   contains 

  subroutine apply_reflective_boundaries(state)
      type(state_2d_type), intent(inout) :: state
      integer :: nx, ny
      nx = state%grid%nx
      ny = state%grid%ny

      ! Reflective in X direction
      state%water_height(1, :) = state%water_height(2, :)
      state%water_height(nx, :) = state%water_height(nx - 1, :)
      state%x_momentum(1, :) = -state%x_momentum(2, :)
      state%x_momentum(nx, :) = -state%x_momentum(nx - 1, :)
      state%y_momentum(1, :) = state%y_momentum(2, :)
      state%y_momentum(nx, :) = state%y_momentum(nx - 1, :)

      ! Reflective in Y direction
      state%water_height(:, 1) = state%water_height(:, 2)
      state%water_height(:, ny) = state%water_height(:, ny - 1)
      state%x_momentum(:, 1) = state%x_momentum(:, 2)
      state%x_momentum(:, ny) = state%x_momentum(:, ny - 1)
      state%y_momentum(:, 1) = -state%y_momentum(:, 2)
      state%y_momentum(:, ny) = -state%y_momentum(:, ny - 1)

      ! Corner values (to avoid double overwrite)
      state%water_height(1, 1) = state%water_height(2, 2)
      state%x_momentum(1, 1) = -state%x_momentum(2, 2)
      state%y_momentum(1, 1) = -state%y_momentum(2, 2)

      state%water_height(1, ny) = state%water_height(2, ny - 1)
      state%x_momentum(1, ny) = -state%x_momentum(2, ny - 1)
      state%y_momentum(1, ny) = -state%y_momentum(2, ny - 1)

      state%water_height(nx, 1) = state%water_height(nx - 1, 2)
      state%x_momentum(nx, 1) = -state%x_momentum(nx - 1, 2)
      state%y_momentum(nx, 1) = -state%y_momentum(nx - 1, 2)

      state%water_height(nx, ny) = state%water_height(nx - 1, ny - 1)
      state%x_momentum(nx, ny) = -state%x_momentum(nx - 1, ny - 1)
      state%y_momentum(nx, ny) = -state%y_momentum(nx - 1, ny - 1)

   end subroutine apply_reflective_boundaries


   end module pic_boundaries