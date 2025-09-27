module pic_swe_boundaries
   use pic_types, only: default_int, dp
   use pic_swe_state_2d, only: state_2d_type
   implicit none

   private
   public :: apply_reflective_boundaries

contains

   subroutine apply_reflective_boundaries(state)
      type(state_2d_type), intent(inout) :: state
      integer :: nx, ny
      integer :: i, j
      nx = state%grid%nx
      ny = state%grid%ny

!$omp target teams loop collapse(2) bind(teams) thread_limit(128)
      do j = 1, ny
         do i = 1, nx
            ! Corners first (unique values)
            if (i == 1 .and. j == 1) then
               state%water_height(1, 1) = state%water_height(2, 2)
               state%x_momentum(1, 1) = -state%x_momentum(2, 2)
               state%y_momentum(1, 1) = -state%y_momentum(2, 2)

            else if (i == 1 .and. j == ny) then
               state%water_height(1, ny) = state%water_height(2, ny - 1)
               state%x_momentum(1, ny) = -state%x_momentum(2, ny - 1)
               state%y_momentum(1, ny) = -state%y_momentum(2, ny - 1)

            else if (i == nx .and. j == 1) then
               state%water_height(nx, 1) = state%water_height(nx - 1, 2)
               state%x_momentum(nx, 1) = -state%x_momentum(nx - 1, 2)
               state%y_momentum(nx, 1) = -state%y_momentum(nx - 1, 2)

            else if (i == nx .and. j == ny) then
               state%water_height(nx, ny) = state%water_height(nx - 1, ny - 1)
               state%x_momentum(nx, ny) = -state%x_momentum(nx - 1, ny - 1)
               state%y_momentum(nx, ny) = -state%y_momentum(nx - 1, ny - 1)

               ! Left/right edges (exclude corners)
            else if (i == 1) then
               state%water_height(1, j) = state%water_height(2, j)
               state%x_momentum(1, j) = -state%x_momentum(2, j)
               state%y_momentum(1, j) = state%y_momentum(2, j)

            else if (i == nx) then
               state%water_height(nx, j) = state%water_height(nx - 1, j)
               state%x_momentum(nx, j) = -state%x_momentum(nx - 1, j)
               state%y_momentum(nx, j) = state%y_momentum(nx - 1, j)

               ! Bottom/top edges (exclude corners)
            else if (j == 1) then
               state%water_height(i, 1) = state%water_height(i, 2)
               state%x_momentum(i, 1) = state%x_momentum(i, 2)
               state%y_momentum(i, 1) = -state%y_momentum(i, 2)

            else if (j == ny) then
               state%water_height(i, ny) = state%water_height(i, ny - 1)
               state%x_momentum(i, ny) = state%x_momentum(i, ny - 1)
               state%y_momentum(i, ny) = -state%y_momentum(i, ny - 1)
            end if
         end do
      end do
!$omp end target teams loop

!      ! Reflective in X direction
!      !$omp target
!      state%water_height(1, :) = state%water_height(2, :)
!      state%water_height(nx, :) = state%water_height(nx - 1, :)
!      state%x_momentum(1, :) = -state%x_momentum(2, :)
!      state%x_momentum(nx, :) = -state%x_momentum(nx - 1, :)
!      state%y_momentum(1, :) = state%y_momentum(2, :)
!      state%y_momentum(nx, :) = state%y_momentum(nx - 1, :)
!
!      ! Reflective in Y direction
!      state%water_height(:, 1) = state%water_height(:, 2)
!      state%water_height(:, ny) = state%water_height(:, ny - 1)
!      state%x_momentum(:, 1) = state%x_momentum(:, 2)
!      state%x_momentum(:, ny) = state%x_momentum(:, ny - 1)
!      state%y_momentum(:, 1) = -state%y_momentum(:, 2)
!      state%y_momentum(:, ny) = -state%y_momentum(:, ny - 1)
!
!      ! Corner values (to avoid double overwrite)
!      state%water_height(1, 1) = state%water_height(2, 2)
!      state%x_momentum(1, 1) = -state%x_momentum(2, 2)
!      state%y_momentum(1, 1) = -state%y_momentum(2, 2)
!
!      state%water_height(1, ny) = state%water_height(2, ny - 1)
!      state%x_momentum(1, ny) = -state%x_momentum(2, ny - 1)
!      state%y_momentum(1, ny) = -state%y_momentum(2, ny - 1)
!
!      state%water_height(nx, 1) = state%water_height(nx - 1, 2)
!      state%x_momentum(nx, 1) = -state%x_momentum(nx - 1, 2)
!      state%y_momentum(nx, 1) = -state%y_momentum(nx - 1, 2)
!
!      state%water_height(nx, ny) = state%water_height(nx - 1, ny - 1)
!      state%x_momentum(nx, ny) = -state%x_momentum(nx - 1, ny - 1)
!      state%y_momentum(nx, ny) = -state%y_momentum(nx - 1, ny - 1)
!      !$omp end target

   end subroutine apply_reflective_boundaries

end module pic_swe_boundaries
