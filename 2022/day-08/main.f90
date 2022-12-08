program main
   use day08
   implicit none

   integer :: sample_grid_size = 5
   integer :: sample_grid(5, 5)
   ! integer :: grid_size = 99
   ! integer :: grid(99, 99)

   sample_grid = read_file("day08.sample", sample_grid_size)
   print *, "Sample grid:"
   call print_grid(sample_grid)

   ! grid = read_file("day08.in", grid_size)
   ! print *, "Grid:"
   ! call print_grid(grid)

end program main
