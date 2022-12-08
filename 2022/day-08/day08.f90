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

end module day08
