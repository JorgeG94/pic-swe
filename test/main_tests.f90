program pic_tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
   & select_suite, run_selected, get_argument
   use test_suite1, only: collect_suite1
   use test_grid_2d, only: collect_tests_grid_2d
   use test_state, only: collect_tests_state
   use test_timestep, only: collect_tests_timestep
   use test_boundaries, only: collect_tests_boundaries
   use test_fluxes, only: collect_tests_fluxes
   ! add here the module you want to test
   implicit none
   integer :: stat, is
   integer, parameter :: ntest_suites = 6
    !! number of tests, this number needs to be modified and equal to the number of files we have with unit tests
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0
   allocate (testsuites(ntest_suites))
   ! here you add another test suite to the array
   testsuites(1) = new_testsuite("base_utils", collect_suite1)
   testsuites(2) = new_testsuite("grid_2d", collect_tests_grid_2d)
   testsuites(3) = new_testsuite("state_2d", collect_tests_state)
   testsuites(4) = new_testsuite("timestep", collect_tests_timestep)
   testsuites(5) = new_testsuite("boundaries", collect_tests_boundaries)
   testsuites(6) = new_testsuite("fluxes", collect_tests_fluxes)

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) then
               error stop 1
            end if
         else
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, fmt) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, fmt) "Testing all:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
      error stop 1
   end if

end program pic_tester
