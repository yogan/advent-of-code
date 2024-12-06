module aoc
   implicit none

   type :: position
      integer :: r
      integer :: c
   end type position

contains

   subroutine read_file(filename, lines)
      character(len=*), intent(in) :: filename
      character(len=*), dimension(:), allocatable, intent(out) :: lines
      integer :: unit, status, i, rows = 0

      open(newunit=unit, file=filename, status='old', action='read', iostat=status)
      if (status /= 0) then
         write(*, '(A, A)') "Error: Unable to open file: ", trim(filename)
         return
      end if

      do
         read(unit, '(A)', iostat=status)
         if (status /= 0) exit
         rows = rows + 1
      end do

      rewind(unit)
      allocate(lines(rows))

      do i = 1, rows
         read(unit, '(A)', iostat=status) lines(i)
         if (status /= 0) exit
      end do

      close(unit)
   end subroutine read_file

   subroutine find_guard(lab, r, c)
      character(len=*), dimension(:), intent(in) :: lab
      integer, intent(out) :: r, c

      do r = 1, size(lab)
         do c = 1, len(lab(r))
            if (lab(r)(c:c) == '^') return
         end do
      end do
   end subroutine find_guard

   subroutine part1(lab, visited, visited_size)
      character(len=*), dimension(:), intent(in) :: lab
      type(position), dimension(:), intent(inout) :: visited
      integer, intent(inout) :: visited_size
      integer :: r, c, d_tmp, dr = -1, dc = 0
      type(position) :: pos

      call find_guard(lab, r, c)

      pos = position(r, c)
      call add_to_set(visited, visited_size, pos)

      do
         if (r+dr < 1 .or. r+dr > size(lab) .or. c+dc < 1 .or. c+dc > len(lab(r))) then
            exit
         end if
         if (lab(r+dr)(c+dc:c+dc) == '#') then
            d_tmp = dr
            dr = dc
            dc = -d_tmp
         else
            r = r + dr
            c = c + dc
            pos = position(r, c)
            call add_to_set(visited, visited_size, pos)
         end if
      end do
   end subroutine part1

   subroutine add_to_set(set, size, pos)
      type(position), dimension(:), intent(inout) :: set
      integer, intent(inout) :: size
      type(position), intent(in) :: pos
      integer :: i

      do i = 1, size
         if (set(i)%r == pos%r .and. set(i)%c == pos%c) return
      end do

      size = size + 1
      set(size) = pos
   end subroutine add_to_set

end module aoc
