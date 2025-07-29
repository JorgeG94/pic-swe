add_subdirectory(grid)
target_sources(${main_lib} PRIVATE pic_boundaries.f90  pic_constants.f90  pic_flux_2d.f90  pic_gpu_utilities.f90  pic_shallow_water_driver.f90  pic_state.f90  pic_timestep.f90  pic_update_2d.f90)
