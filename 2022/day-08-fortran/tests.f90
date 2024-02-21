program tests
   use day08
   implicit none

   call test_read_file()
   call test_calc_visibility()
   call test_count_visible_trees()
   call test_find_max_scenic_score()

contains
   subroutine test_read_file()
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
         print *, "Test failed: expected grid:"
         call print_grid(expected_grid)
         print *, "Got grid:"
         call print_grid(grid)
      else
         print *, "Test passed (read_file)"
      end if
   end subroutine test_read_file

   subroutine test_calc_visibility()
      integer :: grid_size = 5
      integer, dimension(5,5):: grid = reshape( &
         (/ 3,0,3,7,3, &
            2,5,5,1,2, &
            6,5,3,3,2, &
            3,3,5,4,9, &
            3,5,3,9,0 /), &
         shape(grid), order=(/2,1/) )

      logical :: visibility(5,5)
      logical, dimension(5,5) :: expected_visibility = reshape( &
         (/ .true.,.true.,.true.,.true.,.true., &
            .true.,.true.,.true.,.false.,.true., &
            .true.,.true.,.false.,.true.,.true., &
            .true.,.false.,.true.,.false.,.true., &
            .true.,.true.,.true.,.true.,.true. /), &
         shape(expected_visibility), order=(/2,1/) )

      visibility = calc_visibility(grid, grid_size)

      if (any(visibility .neqv. expected_visibility)) then
         print *, "Test failed: expected visibility:"
         call print_visibility(expected_visibility)
         print *, "Got visibility:"
         call print_visibility(visibility)
      else
         print *, "Test passed (calc_visibility)"
      end if
   end subroutine test_calc_visibility

   subroutine test_count_visible_trees()
      integer :: number, expected_number = 21
      logical, dimension(5,5) :: visibility = reshape( &
         (/ .true.,.true.,.true.,.true.,.true., &
            .true.,.true.,.true.,.false.,.true., &
            .true.,.true.,.false.,.true.,.true., &
            .true.,.false.,.true.,.false.,.true., &
            .true.,.true.,.true.,.true.,.true. /), &
         shape(visibility), order=(/2,1/) )

      number = count_visible_trees(visibility)

      if (number /= expected_number) then
         print *, "Test failed: expected tree count:" &
            , expected_number, ", got tree count:", number
      else
         print *, "Test passed (count_visible_trees)"
      end if
   end subroutine test_count_visible_trees

   subroutine test_find_max_scenic_score()
      integer :: grid_size = 5, max_score
      integer, dimension(5,5):: grid = reshape( &
         (/ 3,0,3,7,3, &
            2,5,5,1,2, &
            6,5,3,3,2, &
            3,3,5,4,9, &
            3,5,3,9,0 /), &
         shape(grid), order=(/2,1/) )

      max_score = find_max_scenic_score(grid, grid_size)

      if (max_score /= 8) then
         print *, "Test failed: expected max score of 8", &
            ", got tree count:", max_score
      else
         print *, "Test passed (find_max_scenic_score)"
      end if
   end subroutine test_find_max_scenic_score

end program tests
