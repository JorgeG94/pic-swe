module pic_flux_2d
   use pic_types
   use pic_state_2d
   use local_pic_constants, only: gravity
   implicit none

contains


   subroutine compute_rusanov_flux_x(state, flux_h, flux_hu, flux_hv)
      type(state_2d_type), intent(in) :: state
      real(dp), allocatable, intent(out) :: flux_h(:,:), flux_hu(:,:), flux_hv(:,:)
      real(dp), parameter :: epsilon = 1.0e-3_dp

      workspace: block

         integer(default_int) :: nx, ny

         nx = state%grid%nx
         ny = state%grid%ny

         allocate(flux_h(nx+1, ny), flux_hu(nx+1,ny), flux_hv(nx+1,ny))
         flux_loop: block
            real(dp) :: h_L, hu_L, hv_L, u_L, v_L, c_L
            real(dp) :: h_R, hu_R, hv_R, u_R, v_R, c_R
            real(dp) :: a_max
            real(dp) :: flux_L(3), flux_R(3), flux(3)
            integer(default_int) :: i,j
!    do concurrent (j = 1:ny, i = 1:nx-1)
!$omp parallel do collapse(2) default(shared) private(i, j, h_L, hu_L, hv_L, h_R, hu_R, hv_R, u_L, v_L, u_R, v_R, flux_L, flux_R, c_L, c_R, a_max, flux)
            do j = 1, ny
               do i = 1, nx-1

                  h_L = state%water_height(i,j)
                  hu_L = state%x_momentum(i,j)
                  hv_L = state%y_momentum(i,j)

                  h_R = state%water_height(i+1,j)
                  hu_R = state%x_momentum(i+1,j)
                  hv_R = state%y_momentum(i+1,j)

                  ! if(h_L < epsilon .and. h_R < epsilon) then
                  !    flux_h(i+1, j) = 0.0_dp
                  !    flux_hu(i+1, j) = 0.0_dp
                  !    flux_hv(i+1, j) = 0.0_dp
                  ! else
                  if (h_L < epsilon) h_L = epsilon
                  if (h_R < epsilon) h_R = epsilon
                  u_L = merge(hu_L / h_L, 0.0_dp, h_L > epsilon)
                  v_L = merge(hv_L / h_L, 0.0_dp, h_L > epsilon)
                  u_R = merge(hu_R / h_R, 0.0_dp, h_R > epsilon)
                  v_R = merge(hv_R / h_R, 0.0_dp, h_R > epsilon)


                  flux_L = [ hu_L, hu_L*u_L + 0.5_dp * gravity * h_L**2, hu_L*v_L]
                  flux_R = [ hu_R, hu_R*u_R + 0.5_dp * gravity * h_R**2, hu_R*v_R]

                  c_L = abs(u_L) + sqrt(gravity * h_L)
                  c_R = abs(u_R) + sqrt(gravity * h_R)

                  a_max = max(c_L, c_R)

                  flux = 0.5_dp * (flux_L + flux_R) - 0.5_dp * a_max * ([h_R, hu_R, hv_R] - [h_L, hu_L, hv_L])

                  flux_h(i+1,j) = flux(1)
                  flux_hu(i+1,j) = flux(2)
                  flux_hv(i+1,j) = flux(3)
                  ! endif

               end do
            end do
            !$omp end parallel do
         end block flux_loop


         flux_h(1,:) = 0.0_dp;    flux_h(nx+1,:) = 0.0_dp
         flux_hu(1,:) = 0.0_dp;   flux_hu(nx+1,:) = 0.0_dp
         flux_hv(1,:) = 0.0_dp;   flux_hv(nx+1,:) = 0.0_dp

      end block workspace

   end subroutine compute_rusanov_flux_x

   subroutine compute_rusanov_flux_y(state, flux_h, flux_hu, flux_hv)
      type(state_2d_type), intent(in) :: state
      real(dp), allocatable, intent(out) :: flux_h(:,:), flux_hu(:,:), flux_hv(:,:)
      real(dp), parameter :: epsilon = 1.0e-3_dp

      workspace: block

         integer(default_int) :: nx, ny

         nx = state%grid%nx
         ny = state%grid%ny

         allocate(flux_h(nx, ny+1), flux_hu(nx,ny+1), flux_hv(nx,ny+1))
         flux_loop: block
            real(dp) :: h_L, hu_L, hv_L, u_L, v_L, c_L
            real(dp) :: h_R, hu_R, hv_R, u_R, v_R, c_R
            real(dp) :: a_max
            integer(default_int) :: i,j
            real(dp) :: flux_L(3), flux_R(3), flux(3)
            !do concurrent (j = 1:ny-1, i = 1:nx)
            !$omp parallel do collapse(2) default(shared) private(i, j, h_L, hu_L, hv_L, h_R, hu_R, hv_R, u_L, v_L, u_R, v_R, flux_L, flux_R, c_L, c_R, a_max, flux)
            do j = 1, ny - 1
               do i = 1, nx
                  !do j = 1, ny
                  !do i = 1, nx

                  h_L = state%water_height(i,j)
                  hu_L = state%x_momentum(i,j)
                  hv_L = state%y_momentum(i,j)

                  h_R = state%water_height(i,j+1)
                  hu_R = state%x_momentum(i,j+1)
                  hv_R = state%y_momentum(i,j+1)

                  ! if(h_L < epsilon .and. h_R < epsilon) then
                  !    flux_h(i, j+1) = 0.0_dp
                  !    flux_hu(i, j+1) = 0.0_dp
                  !    flux_hv(i, j+1) = 0.0_dp
                  ! else
                  if (h_L < epsilon) h_L = epsilon
                  if (h_R < epsilon) h_R = epsilon

                  u_L = merge(hu_L / h_L, 0.0_dp, h_L > epsilon)
                  v_L = merge(hv_L / h_L, 0.0_dp, h_L > epsilon)
                  u_R = merge(hu_R / h_R, 0.0_dp, h_R > epsilon)
                  v_R = merge(hv_R / h_R, 0.0_dp, h_R > epsilon)


                  flux_L = [ hu_L, hu_L*u_L + 0.5_dp * gravity * h_L**2, hu_L*v_L]
                  flux_R = [ hu_R, hu_R*u_R + 0.5_dp * gravity * h_R**2, hu_R*v_R]

                  c_L = abs(u_L) + sqrt(gravity * h_L)
                  c_R = abs(u_R) + sqrt(gravity * h_R)

                  a_max = max(c_L, c_R)

                  flux = 0.5_dp * (flux_L + flux_R) - 0.5_dp * a_max * ([h_R, hu_R, hv_R] - [h_L, hu_L, hv_L])

                  flux_h(i,j+1) = flux(1)
                  flux_hu(i,j+1) = flux(2)
                  flux_hv(i,j+1) = flux(3)
                  ! endif

               end do
            end do
            !$omp end parallel do
         end block flux_loop


         flux_h(:,1) = 0.0_dp;    flux_h(:,ny+1) = 0.0_dp
         flux_hu(:,1) = 0.0_dp;   flux_hu(:,ny+1) = 0.0_dp
         flux_hv(:,1) = 0.0_dp;   flux_hv(:,ny+1) = 0.0_dp

      end block workspace

   end subroutine compute_rusanov_flux_y
end module pic_flux_2d
