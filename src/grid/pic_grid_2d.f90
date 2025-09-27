module pic_swe_grid_2d
   use pic_types, only: default_int, dp, sp
   implicit none

   type :: grid_2d_type
      real(dp) :: xmin, xmax
      real(dp) :: ymin, ymax

      real(dp) :: dx, dy

      integer(default_int) :: nx, ny

      real(dp), allocatable :: x(:), y(:)
      real(dp), allocatable :: x2d(:, :), y2d(:, :)

      logical :: is_initialized = .false.
   end type grid_2d_type

contains

   subroutine init_grid(grid, xmin, xmax, ymin, ymax, dx, dy)
      type(grid_2d_type), intent(out) :: grid
      real(dp), intent(in) :: xmin, xmax, ymin, ymax, dx
      real(dp), intent(in), optional :: dy

      grid%xmin = xmin
      grid%xmax = xmax
      grid%ymin = ymin
      grid%ymax = ymax
      grid%dx = dx
      if (present(dy)) then
         grid%dy = dy
      else
         grid%dy = dx
      end if

      grid%nx = int(((xmax - xmin)/grid%dx) + 1, default_int)
      grid%ny = int(((ymax - ymin)/grid%dy) + 1, default_int)

      allocate (grid%x(grid%nx), grid%y(grid%ny))

      block
         integer(default_int) :: i, j

         do i = 1, grid%nx
            grid%x(i) = xmin + (i - 1)*grid%dx
         end do
         do i = 1, grid%ny
            grid%y(i) = ymin + (i - 1)*grid%dy
         end do

         grid%is_initialized = .true.

      end block
   end subroutine init_grid

   subroutine generate_2d_grids(grid)
      type(grid_2d_type), intent(inout) :: grid

      if (.not. grid%is_initialized) then
         error stop "grid has not been initialized!"
      end if

      allocate (grid%x2d(grid%nx, grid%ny), grid%y2d(grid%nx, grid%ny))

      block
         integer(default_int) :: i, j

         do j = 1, grid%ny
            do i = 1, grid%nx
               grid%x2d(i, j) = grid%x(i)
               grid%y2d(i, j) = grid%y(j)
            end do
         end do
      end block

   end subroutine generate_2d_grids

   pure function get_grid_coords(grid, i, j) result(coord)
      type(grid_2d_type), intent(in) :: grid
      integer(default_int), intent(in) :: i, j
      real(dp), dimension(2) :: coord

      coord = [grid%x(i), grid%y(j)]
   end function get_grid_coords

end module pic_swe_grid_2d
