module local_pic_constants
   use pic_types, only: sp,dp
   implicit none
   private
   real(dp), parameter, public :: gravity = 9.81_dp
   real(dp), parameter, public :: epsilon = 1.0e-3_dp

end module local_pic_constants
