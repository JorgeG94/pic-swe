module pic_flux_2d
   use pic_types
   use pic_state_2d
   use local_pic_constants, only: gravity, epsilon
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
      use_omp = .false.
    end if

    if(use_omp) then 
    ! in theory this could be good for the GPU
    nx = size(fluxes%flux_h,1)
    ny = size(fluxes%flux_h,2)
    !$omp target teams distribute parallel do collapse(2) private(i,j) default(shared)
    do i = 1, nx
      do j = 1, ny 
        fluxes%flux_h(i,j) = a
        fluxes%flux_hu(i,j) = a
        fluxes%flux_hv(i,j) = a
      end do 
    end do 
    !$omp end target teams distribute parallel do
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
      type(state_2d_type), intent(in) :: state
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
            real(dp) :: a_max
            real(dp) :: flux_L(3), flux_R(3), flux(3)
            integer(default_int) :: i, j, ii, jj, j_start, i_start
            integer(default_int) :: i_end, j_end
            integer(default_int), parameter :: bx = 32, by = 32
            real(dp), parameter :: sqroot_gravity = sqrt(gravity)

!$omp target teams distribute parallel do collapse(2) private(i, j, ii, jj, h_L, h_R, hu_L, hu_R, hv_L, hv_R, u_L, u_R, v_L, v_R, flux_L, flux_R, flux, c_L, c_R, a_max)
            do jj = 1, ny - 1, by
               do ii = 1, nx - 1, bx
                  do j = jj, min(jj + by - 1, ny - 1)
                     do i = ii, min(ii + bx - 1, nx - 1)
                        h_L = state%water_height(i, j)
                        hu_L = state%x_momentum(i, j)
                        hv_L = state%y_momentum(i, j)
                        h_R = state%water_height(i + 1, j)
                        hu_R = state%x_momentum(i + 1, j)
                        hv_R = state%y_momentum(i + 1, j)
                        ! this can be a subroutine
                        call compute_velocity(h_L, hu_L, hv_L, u_L, v_L)
                        call compute_velocity(h_R, hu_R, hv_R, u_R, v_R)

                        flux_L = [hu_L, hu_L*u_L + 0.5_dp*gravity*h_L**2, hu_L*v_L]
                        flux_R = [hu_R, hu_R*u_R + 0.5_dp*gravity*h_R**2, hu_R*v_R]

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

                        !end block x_flux

                        !y_flux: block
                        h_L = state%water_height(i, j)
                        hu_L = state%x_momentum(i, j)
                        hv_L = state%y_momentum(i, j)
                        h_R = state%water_height(i, j + 1)
                        hu_R = state%x_momentum(i, j + 1)
                        hv_R = state%y_momentum(i, j + 1)
                        ! this can be a subroutine
                        call compute_velocity(h_L, hu_L, hv_L, u_L, v_L)
                        call compute_velocity(h_R, hu_R, hv_R, u_R, v_R)

                        flux_L = [hu_L, hu_L*u_L + 0.5_dp*gravity*h_L**2, hu_L*v_L]
                        flux_R = [hu_R, hu_R*u_R + 0.5_dp*gravity*h_R**2, hu_R*v_R]

                        c_L = abs(u_L) + sqroot_gravity*sqrt(h_L)
                        c_R = abs(u_R) + sqroot_gravity*sqrt(h_R)

                        a_max = max(c_L, c_R)

                        !flux = 0.5_dp * (flux_L + flux_R) - 0.5_dp * a_max * ([h_R, hu_R, hv_R] - [h_L, hu_L, hv_L])
                        flux(1) = 0.5_dp*(flux_L(1) + flux_R(1)) - 0.5_dp*a_max*(h_R - h_L)
                        flux(2) = 0.5_dp*(flux_L(2) + flux_R(2)) - 0.5_dp*a_max*(hu_R - hu_L)
                        flux(3) = 0.5_dp*(flux_L(3) + flux_R(3)) - 0.5_dp*a_max*(hv_R - hv_L)

                        flux_y%flux_h(i, j + 1) = flux(1)
                        flux_y%flux_hu(i, j + 1) = flux(2)
                        flux_y%flux_hv(i, j + 1) = flux(3)

!              end block y_flux

                     end do
                  end do
               end do
            end do
            !$omp end target teams distribute parallel do

!            end do

!            !$pragma omp end parallel do simd

         end block flux_loop

         flux_y%flux_h(:, 1) = 0.0_dp;  flux_y%flux_h(:, ny + 1) = 0.0_dp
         flux_y%flux_hu(:, 1) = 0.0_dp; flux_y%flux_hu(:, ny + 1) = 0.0_dp
         flux_y%flux_hv(:, 1) = 0.0_dp; flux_y%flux_hv(:, ny + 1) = 0.0_dp

         flux_x%flux_h(1, :) = 0.0_dp;  flux_x%flux_h(nx + 1, :) = 0.0_dp
         flux_x%flux_hu(1, :) = 0.0_dp; flux_x%flux_hu(nx + 1, :) = 0.0_dp
         flux_x%flux_hv(1, :) = 0.0_dp; flux_x%flux_hv(nx + 1, :) = 0.0_dp

      end block workspace
   contains

      pure subroutine compute_velocity(h, hu, hv, u, v)
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

end module pic_flux_2d
