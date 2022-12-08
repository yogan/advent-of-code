module day08
   implicit none
contains

   function read_file( filename, grid_size ) result(grid)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: grid_size
      integer :: i, j, n
      integer :: grid(grid_size, grid_size)
      character(len=grid_size) :: line

      open( unit=1, file=filename, status='old' )
      do i = 1, grid_size
         read( 1, '(a)', iostat=n ) line
         if ( n /= 0 ) exit
         do j = 1, grid_size
            read(line(j:j), *) grid(i,j)
         end do
      end do
      close( 1 )

      grid = grid
   end function read_file

   function calc_visibility( grid, grid_size ) result(visibility)
      integer, intent(in) :: grid(:,:)
      integer, intent(in) :: grid_size
      logical :: visibility(grid_size, grid_size)
      integer :: row, col, x

      do row = 1, grid_size
         do col = 1, grid_size
            visibility(row, col) = .true.
         end do
      end do

      do row = 2, grid_size - 1
         loop: do col = 2, grid_size - 1
            visibility(row, col) = .false.

            ! go to the left
            do x = col - 1, 1, -1
               if ( grid(row, x) >= grid(row, col) ) then
                  go to 10
               end if
            end do
            visibility(row, col) = .true.
            cycle loop

            ! go to the right
10          do x = col + 1, grid_size, 1
               if ( grid(row, x) >= grid(row, col) ) then
                  go to 20
               end if
            end do
            visibility(row, col) = .true.
            cycle loop

            ! go up
20          do x = row - 1, 1, -1
               if ( grid(x, col) >= grid(row, col) ) then
                  go to 30
               end if
            end do
            visibility(row, col) = .true.
            cycle loop

            ! go down
30          do x = row + 1, grid_size, 1
               if ( grid(x, col) >= grid(row, col) ) then
                  cycle loop
               end if
            end do
            visibility(row, col) = .true.
         end do loop
      end do

      visibility = visibility
   end function calc_visibility

   function find_max_scenic_score( grid, grid_size ) result(max_score)
      integer, intent(in) :: grid(:,:)
      integer, intent(in) :: grid_size
      integer :: row, col, x, score, max_score
      integer :: dist_left, dist_right, dist_up, dist_down

      max_score = 0

      do row = 2, grid_size - 1
         do col = 2, grid_size - 1
            dist_left = 0
            dist_right = 0
            dist_up = 0
            dist_down = 0
            score = 0

            ! go to the left
            do x = col - 1, 1, -1
               dist_left = dist_left + 1
               if ( grid(row, x) >= grid(row, col) ) then
                  go to 10
               end if
            end do

            ! go to the right
10          do x = col + 1, grid_size, 1
               dist_right = dist_right + 1
               if ( grid(row, x) >= grid(row, col) ) then
                  go to 20
               end if
            end do

            ! go up
20          do x = row - 1, 1, -1
               dist_up = dist_up + 1
               if ( grid(x, col) >= grid(row, col) ) then
                  go to 30
               end if
            end do

            ! go down
30          do x = row + 1, grid_size, 1
               dist_down = dist_down + 1
               if ( grid(x, col) >= grid(row, col) ) then
                  go to 40
               end if
            end do

40          score = dist_left * dist_right * dist_up * dist_down
            if ( score > max_score ) then
               max_score = score
            end if
         end do
      end do
   end function find_max_scenic_score

   function count_visible_trees( visibility ) result(number)
      logical, intent(in) :: visibility(:,:)
      integer :: number

      number = count( visibility )
   end function count_visible_trees

   subroutine print_grid( grid )
      integer, intent(in) :: grid(:,:)
      integer :: row, col

      do row = 1, size(grid, 2)
         write(*, fmt="(1x,a)", advance="no") "  "
         do col = 1, size(grid, 1)
            write(*, fmt="(i0)", advance="no") grid(row, col)
         end do
         print *, ''
      end do
   end subroutine print_grid

   subroutine print_visibility( visibility )
      logical, intent(in) :: visibility(:,:)
      integer :: row, col

      do row = 1, size(visibility, 2)
         write(*, fmt="(1x,a)", advance="no") "  "
         do col = 1, size(visibility, 1)
            write(*, fmt="(L)", advance="no") visibility(row, col)
         end do
         print *, ''
      end do
   end subroutine print_visibility

end module day08
