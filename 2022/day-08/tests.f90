program tests
   use day08
   implicit none

   integer :: grid_size = 5
   integer :: grid(5,5)

   integer, dimension(5,5) :: expected_grid = reshape( &
      (/ 3,0,3,7,3, &
         2,5,5,1,2, &
         6,5,3,3,2, &
         3,3,5,4,9, &
         3,5,3,9,0 /), &
      shape(expected_grid), order=(/2,1/) )

   grid = read_file("day08.sample", grid_size)
   if (any(grid /= expected_grid)) then
      print *, "Test failed: Expected grid :"
      call print_grid(expected_grid)
      print *, "Got grid :"
      call print_grid(grid)
   end if

end program tests
