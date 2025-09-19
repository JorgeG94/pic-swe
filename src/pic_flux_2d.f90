module pic_swe_flux_2d
   use pic_types
   use pic_swe_state_2d
   use pic_swe_constants, only: epsilon
   use pic_constants, only: gravity
   implicit none

  type :: flux_type 

  real(dp), allocatable :: flux_h(:,:)
  real(dp), allocatable :: flux_hu(:,:)
  real(dp), allocatable :: flux_hv(:,:)

  contains 

  procedure :: allocate_fluxes
  procedure :: set_fluxes
  procedure :: deallocate_fluxes

  end type flux_type 

contains

  subroutine allocate_fluxes(fluxes, nx, ny)
    class(flux_type), intent(inout) :: fluxes 
    integer(default_int), intent(in) :: nx, ny 

    allocate(fluxes%flux_h(nx,ny))
    allocate(fluxes%flux_hu(nx,ny))
    allocate(fluxes%flux_hv(nx,ny))

  end subroutine allocate_fluxes

  subroutine set_fluxes(fluxes, a, threaded)
    class(flux_type), intent(inout) :: fluxes 
    real(dp), intent(in) :: a 
    logical, intent(in), optional :: threaded
    logical :: use_omp
    integer(int64) :: nx, ny
    integer(int64) :: i, j

    if(present(threaded)) then 
      use_omp = threaded
    else
      use_omp = .true.
    end if

     if(use_omp) then 
     ! in theory this could be good for the GPU
     nx = size(fluxes%flux_h,1)
     ny = size(fluxes%flux_h,2)
     !$omp target teams distribute parallel do simd collapse(2) private(i,j) default(shared) & 
     !$omp map(to:fluxes, fluxes%flux_h, fluxes%flux_hu, fluxes%flux_hv)
     do i = 1, nx
       do j = 1, ny 
         fluxes%flux_h(i,j) = a
         fluxes%flux_hu(i,j) = a
         fluxes%flux_hv(i,j) = a
       end do 
     end do 
     !$omp end target teams distribute parallel do simd
     else 
    fluxes%flux_h = a
    fluxes%flux_hu = a
    fluxes%flux_hv = a
    endif 


  end subroutine set_fluxes

  subroutine deallocate_fluxes(fluxes)
    class(flux_type), intent(inout) :: fluxes 

    deallocate(fluxes%flux_h)
    deallocate(fluxes%flux_hu)
    deallocate(fluxes%flux_hv)
  end subroutine deallocate_fluxes

  ! subroutine compute_rusanov_fluxes_xy(state, flux_x_h, flux_x_hu, flux_x_hv, flux_y_h, flux_y_hu, flux_y_hv)
  subroutine compute_rusanov_fluxes_xy(state, flux_x, flux_y)
      type(state_2d_type), intent(inout) :: state
      type(flux_type), intent(inout) :: flux_x, flux_y
      !real(dp), allocatable, intent(inout) :: flux_x_h(:, :), flux_x_hu(:, :), &
      !                                      flux_x_hv(:, :), flux_y_h(:, :), flux_y_hu(:, :), flux_y_hv(:, :)

      workspace: block

         integer(default_int) :: nx, ny

         nx = state%grid%nx
         ny = state%grid%ny

         !allocate (flux_x_h(nx + 1, ny), flux_x_hu(nx + 1, ny), flux_x_hv(nx + 1, ny))
         !allocate (flux_y_h(nx, ny + 1), flux_y_hu(nx, ny + 1), flux_y_hv(nx, ny + 1))

         flux_loop: block
            real(dp) :: h_L, hu_L, hv_L, u_L, v_L, c_L
            real(dp) :: h_R, hu_R, hv_R, u_R, v_R, c_R
            real(dp) :: y_h_L, y_hu_L, y_hv_L, y_u_L, y_v_L, y_c_L
            real(dp) :: y_h_R, y_hu_R, y_hv_R, y_u_R, y_v_R, y_c_R
            real(dp) :: a_max, y_a_max
            real(dp) :: inv_h_L, inv_h_R
            real(dp) :: flux_L(3), flux_R(3), flux(3)
            real(dp) :: y_flux_L(3), y_flux_R(3), y_flux(3)
            integer(default_int) :: i, j, ii, jj, j_start, i_start
            integer(default_int) :: i_end, j_end
            integer(default_int), parameter :: bx = 32, by = 32
            real(dp), parameter :: sqroot_gravity = sqrt(gravity)
            real(dp), parameter :: half_gravity = 0.5_dp * gravity

            flux_L = 0.0_dp
            flux_R = 0.0_dp  
            flux = 0.0_dp

!$omp target teams loop collapse(2) bind(teams) num_teams(2048) thread_limit(128) &
!$omp private(i, j,  h_L, h_R, hu_L, hu_R, hv_L, hv_R, u_L, u_R, v_L, v_R, flux_L, flux_R, flux, c_L, c_R, a_max) &
!$omp private( y_h_L, y_h_R, y_hu_L, y_hu_R, y_hv_L, y_hv_R, y_u_L, y_u_R, y_v_L, y_v_R, y_flux_L, y_flux_R,  y_c_L, y_c_R, y_a_max)& 
!$omp map(tofrom: state, state%water_height, state%x_momentum, state%y_momentum)& 
!$omp map(tofrom: flux_x, flux_x%flux_h, flux_x%flux_hu, flux_x%flux_hv) &
!$omp map(tofrom: flux_y, flux_y%flux_h, flux_y%flux_hu, flux_y%flux_hv)
                     do j = 1, ny
                      do i = 1, nx
            !do jj = 1, ny - 1, by
               !do ii = 1, nx - 1, bx
                  !do j = jj, min(jj + by - 1, ny - 1)
                     !do i = ii, min(ii + bx - 1, nx - 1)
                        h_L = state%water_height(i, j)
                        hu_L = state%x_momentum(i, j)
                        hv_L = state%y_momentum(i, j)
                        h_R = state%water_height(i + 1, j)
                        hu_R = state%x_momentum(i + 1, j)
                        hv_R = state%y_momentum(i + 1, j)
                        ! this can be a subroutine
                        !call compute_velocity(h_L, hu_L, hv_L, u_L, v_L)
                        !call compute_velocity(h_R, hu_R, hv_R, u_R, v_R)

                        !h_L = max(h_L, epsilon)
                        !inv_h_L = 1.0/h_L
                        !h_R = max(h_R, epsilon)
                        !inv_h_R = 1.0/h_R

                        !u_L = hu_L * inv_h_L
                        !v_L = hv_L * inv_h_L
                        !u_R = hu_R * inv_h_L
                        !v_R = hv_R * inv_h_L


                        if (h_L < epsilon) h_L = epsilon
                        if (h_L > epsilon) then
                          u_L = hu_L/h_L
                          v_L = hv_L/h_L
                        else
                          u_L = 0.0_dp
                          v_L = 0.0_dp
                        end if
                        if (h_R < epsilon) h_R = epsilon
                        if (h_R > epsilon) then
                          u_R = hu_R/h_R
                          v_R = hv_R/h_R
                        else
                          u_R = 0.0_dp
                          v_R = 0.0_dp
                        end if

                        flux_L(1) = hu_L
                        flux_L(2) = hu_L*u_L + half_gravity*h_L**2
                        flux_L(3) = hu_L*v_L
                        
                        flux_R(1) = hu_R
                        flux_R(2) = hu_R*u_R + half_gravity*h_R**2
                        flux_R(3) = hu_R*v_R


                        c_L = abs(u_L) + sqroot_gravity*sqrt(h_L)
                        c_R = abs(u_R) + sqroot_gravity*sqrt(h_R)

                        a_max = max(c_L, c_R)

                        !flux = 0.5_dp * (flux_L + flux_R) - 0.5_dp * a_max * ([h_R, hu_R, hv_R] - [h_L, hu_L, hv_L])
                        flux(1) = 0.5_dp*(flux_L(1) + flux_R(1)) - 0.5_dp*a_max*(h_R - h_L)
                        flux(2) = 0.5_dp*(flux_L(2) + flux_R(2)) - 0.5_dp*a_max*(hu_R - hu_L)
                        flux(3) = 0.5_dp*(flux_L(3) + flux_R(3)) - 0.5_dp*a_max*(hv_R - hv_L)

                        flux_x%flux_h(i + 1, j) = flux(1)
                        flux_x%flux_hu(i + 1, j) = flux(2)
                        flux_x%flux_hv(i + 1, j) = flux(3)
                        !end do
                        !end do 
                        !!$omp end target teams loop

                        !end block x_flux

!!$omp target teams loop collapse(2) bind(teams) num_teams(2048) thread_limit(128) &
!!$omp private(i, j,  h_L, h_R, hu_L, hu_R, hv_L, hv_R, u_L, u_R, v_L, v_R, flux_L, flux_R, flux, c_L, c_R, a_max) &
!!$omp private( y_h_L, y_h_R, y_hu_L, y_hu_R, y_hv_L, y_hv_R, y_u_L, y_u_R, y_v_L, y_v_R, y_flux_L, y_flux_R,  y_c_L, y_c_R, y_a_max)& 
!!$omp map(tofrom: state, state%water_height, state%x_momentum, state%y_momentum)& 
!!$omp map(tofrom: flux_x, flux_x%flux_h, flux_x%flux_hu, flux_x%flux_hv) &
!!$omp map(tofrom: flux_y, flux_y%flux_h, flux_y%flux_hu, flux_y%flux_hv)
!                     do j = 1, ny
!                      do i = 1, nx
                        !y_flux: block
                        y_h_L = state%water_height(i, j)
                        y_hu_L = state%x_momentum(i, j)
                        y_hv_L = state%y_momentum(i, j)
                        y_h_R = state%water_height(i, j + 1)
                        y_hu_R = state%x_momentum(i, j + 1)
                        y_hv_R = state%y_momentum(i, j + 1)
                        ! this can be a subroutine
                        !call compute_velocity(h_L, hu_L, hv_L, u_L, v_L)
                        !call compute_velocity(h_R, hu_R, hv_R, u_R, v_R)
                        if (y_h_L < epsilon) y_h_L = epsilon
                        if (y_h_L > epsilon) then
                          y_u_L = y_hu_L/y_h_L
                          y_v_L = y_hv_L/y_h_L
                        else
                          y_u_L = 0.0_dp
                          y_v_L = 0.0_dp
                        end if
                        if (y_h_R < epsilon) y_h_R = epsilon
                        if (y_h_R > epsilon) then
                          y_u_R = y_hu_R/y_h_R
                          y_v_R = y_hv_R/y_h_R
                        else
                          y_u_R = 0.0_dp
                          y_v_R = 0.0_dp
                        end if


                        ! flux_L = [hu_L, hu_L*u_L + 0.5_dp*gravity*h_L**2, hu_L*v_L]
                        ! flux_R = [hu_R, hu_R*u_R + 0.5_dp*gravity*h_R**2, hu_R*v_R]
                        y_flux_L(1) = y_hu_L
                        y_flux_L(2) = y_hu_L*y_u_L + half_gravity*y_h_L**2
                        y_flux_L(3) = y_hu_L*y_v_L
                        y_flux_R(1) = y_hu_R
                        y_flux_R(2) = y_hu_R*y_u_R + half_gravity*y_h_R**2
                        y_flux_R(3) = y_hu_R*y_v_R



                        y_c_L = abs(y_u_L) + sqroot_gravity*sqrt(y_h_L)
                        y_c_R = abs(y_u_R) + sqroot_gravity*sqrt(y_h_R)

                        y_a_max = max(y_c_L, y_c_R)

                        !flux = 0.5_dp * (flux_L + flux_R) - 0.5_dp * a_max * ([h_R, hu_R, hv_R] - [h_L, hu_L, hv_L])
                        y_flux(1) = 0.5_dp*(y_flux_L(1) + y_flux_R(1)) - 0.5_dp*y_a_max*(y_h_R -  y_h_L)
                        y_flux(2) = 0.5_dp*(y_flux_L(2) + y_flux_R(2)) - 0.5_dp*y_a_max*(y_hu_R - y_hu_L)
                        y_flux(3) = 0.5_dp*(y_flux_L(3) + y_flux_R(3)) - 0.5_dp*y_a_max*(y_hv_R - y_hv_L)

                        flux_y%flux_h(i, j + 1) =  y_flux(1)
                        flux_y%flux_hu(i, j + 1) = y_flux(2)
                        flux_y%flux_hv(i, j + 1) = y_flux(3)

!              end block y_flux

                     !end do
                  !end do
               end do
            end do
            !$omp end target teams loop

!            end do


!$omp target teams loop
do i = 1, size(flux_y%flux_h,1)
   flux_y%flux_h(i,1)      = 0.0_dp
   flux_y%flux_h(i,ny+1)   = 0.0_dp
   flux_y%flux_hu(i,1)     = 0.0_dp
   flux_y%flux_hu(i,ny+1)  = 0.0_dp
   flux_y%flux_hv(i,1)     = 0.0_dp
   flux_y%flux_hv(i,ny+1)  = 0.0_dp
end do

!$omp target teams loop
do j = 1, size(flux_x%flux_h,2)
   flux_x%flux_h(1,j)      = 0.0_dp
   flux_x%flux_h(nx+1,j)   = 0.0_dp
   flux_x%flux_hu(1,j)     = 0.0_dp
   flux_x%flux_hu(nx+1,j)  = 0.0_dp
   flux_x%flux_hv(1,j)     = 0.0_dp
   flux_x%flux_hv(nx+1,j)  = 0.0_dp
end do
         end block flux_loop
!!$omp target 
!         ! Set boundary conditions for fluxes
!         flux_y%flux_h(:, 1) = 0.0_dp  
!         flux_y%flux_h(:, ny + 1) = 0.0_dp
!         flux_y%flux_hu(:, 1) = 0.0_dp  
!         flux_y%flux_hu(:, ny + 1) = 0.0_dp
!         flux_y%flux_hv(:, 1) = 0.0_dp  
!         flux_y%flux_hv(:, ny + 1) = 0.0_dp
!
!         flux_x%flux_h(1, :) = 0.0_dp  
!         flux_x%flux_h(nx + 1, :) = 0.0_dp
!         flux_x%flux_hu(1, :) = 0.0_dp 
!         flux_x%flux_hu(nx + 1, :) = 0.0_dp
!         flux_x%flux_hv(1, :) = 0.0_dp 
!         flux_x%flux_hv(nx + 1, :) = 0.0_dp
!!$omp end target 

      end block workspace
   contains

      pure subroutine compute_velocity(h, hu, hv, u, v)
      !$omp declare target
         real(dp), intent(inout) :: h
         real(dp), intent(in)    :: hu, hv
         real(dp), intent(out)   :: u, v

         if (h < epsilon) h = epsilon
         if (h > epsilon) then
            u = hu/h
            v = hv/h
         else
            u = 0.0_dp
            v = 0.0_dp
         end if
      end subroutine compute_velocity
   end subroutine compute_rusanov_fluxes_xy

end module pic_swe_flux_2d
