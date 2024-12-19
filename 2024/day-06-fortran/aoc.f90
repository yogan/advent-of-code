module aoc
   implicit none

   type :: position
      integer :: r, c
   end type position

   type :: directed_position
      integer :: r, c, dr, dc
   end type directed_position
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

   subroutine travel(lab, guard, visited, visited_size, loops)
      character(len=*), dimension(:), intent(in) :: lab
      type(position), intent(in) :: guard
      type(position), dimension(:), intent(inout) :: visited
      integer, intent(inout) :: visited_size
      logical, intent(inout) :: loops
      type(directed_position), dimension(size(visited)) :: visited_dir
      integer :: r, c, d_tmp, dr, dc, visited_dir_size
      type(position) :: pos
      type(directed_position) :: pos_dir

      loops = .false.
      dr = -1
      dc = 0
      r = guard%r
      c = guard%c
      visited_size = 0
      visited_dir_size = 0

      pos = position(r, c)
      pos_dir = directed_position(r, c, dr, dc)
      call add_to_set(visited, visited_size, pos)
      call add_to_set_dir(visited_dir, visited_dir_size, pos_dir)

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
            pos_dir = directed_position(r, c, dr, dc)
            if (is_in_set_dir(visited_dir, visited_dir_size, pos_dir)) then
               loops = .true.
               exit
            end if
            call add_to_set(visited, visited_size, pos)
            call add_to_set_dir(visited_dir, visited_dir_size, pos_dir)
         end if
      end do
   end subroutine travel

   subroutine add_to_set(set, size, pos)
      type(position), dimension(:), intent(inout) :: set
      integer, intent(inout) :: size
      type(position), intent(in) :: pos
      integer :: i

      if (is_in_set(set, size, pos)) return

      size = size + 1
      set(size) = pos
   end subroutine add_to_set

   subroutine add_to_set_dir(set, size, dir_pos)
      type(directed_position), dimension(:), intent(inout) :: set
      integer, intent(inout) :: size
      type(directed_position), intent(in) :: dir_pos
      integer :: i

      if (is_in_set_dir(set, size, dir_pos)) return

      size = size + 1
      set(size) = dir_pos
   end subroutine add_to_set_dir

   function is_in_set(set, size, pos) result(res)
      type(position), dimension(:), intent(in) :: set
      integer, intent(in) :: size
      type(position), intent(in) :: pos
      logical :: res
      integer :: i

      res = .false.
      do i = 1, size
         if (set(i)%r == pos%r .and. set(i)%c == pos%c) then
            res = .true.
            return
         end if
      end do
   end function is_in_set

   function is_in_set_dir(set, size, dir_pos) result(res)
      type(directed_position), dimension(:), intent(in) :: set
      integer, intent(in) :: size
      type(directed_position), intent(in) :: dir_pos
      logical :: res
      integer :: i

      res = .false.
      do i = 1, size
         if (set(i)%r  == dir_pos%r  .and. set(i)%c  == dir_pos%c .and. &
             set(i)%dr == dir_pos%dr .and. set(i)%dc == dir_pos%dc) then
            res = .true.
            return
         end if
      end do
   end function is_in_set_dir

end module aoc
