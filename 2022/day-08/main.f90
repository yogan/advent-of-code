program main
   use day08
   implicit none

   integer :: sample_grid_size = 5, grid_size = 99
   integer :: sample_grid(5,5), grid(99,99)
   logical :: sample_visibility(5,5), visibility(99,99)

   print *, "SAMPLE"
   sample_grid = read_file("day08.sample", sample_grid_size)
   sample_visibility = calc_visibility(sample_grid, sample_grid_size)
   call print_visibility(sample_visibility)

   print *, ""
   print *, "INPUT"
   grid = read_file("day08.in", grid_size)
   visibility = calc_visibility(grid, grid_size)
   call print_visibility(visibility)

end program main
