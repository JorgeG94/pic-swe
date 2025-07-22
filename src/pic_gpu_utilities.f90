module pic_gpu_utils
use pic_types, only: dp
implicit none 

public :: dasum

interface dasum
procedure :: sum_1d
procedure :: sum_2d 
end interface dasum

contains 

function sum_1d(array) result(res)
!!$omp declare target
real(dp), intent(in) :: array(:)
real(dp) :: res
integer :: i, m 

m = size(array,1)

res = 0.0_dp
!$omp target teams distribute parallel do private(i) default(shared) collapse(1)&
!$omp map(to:array) from(res) reduction(+:res) 
do i = 1, m 
 res = res + array(i)
end do 
!$omp end target teams distribute parallel do 

end function sum_1d 

function sum_2d(array) result(res) 
!!$omp declare target
real(dp),  intent(in) :: array(:,:)
real(dp) :: res 
integer :: i, j 
integer :: n_cols, n_rows 

n_cols = size(array, 1)
n_rows = size(array, 2)
res = 0.0_dp
!$omp target teams distribute parallel do private(i,j) reduction(+:res) collapse(2) &
!$omp map(to:array) map(from:res)
do i = 1, n_cols
  do j = 1, n_rows 
    res = res + array(i,j)
  end do 
end do 
!$omp end target teams distribute parallel do

end function sum_2d

end module pic_gpu_utils 
