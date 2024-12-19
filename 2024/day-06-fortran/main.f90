program main
   use aoc
   implicit none

   character(len=80) :: filename
   character(len=200), dimension(:), allocatable :: lab

   ! pre-allocated, input is 130x130, so this will be more than enough
   type(position), dimension(130*130) :: visited
   type(position), dimension(:), allocatable :: visited_copy
   type(position) :: guard
   integer :: visited_size, i, r, c, num_loops = 0
   logical :: loops

   call get_command_argument(1, filename)
   call read_file(filename, lab)

   call find_guard(lab, r, c)
   guard = position(r, c)

   ! Part 1 is the number of unique positions visited
   call travel(lab, guard, visited, visited_size, loops)
   write(*, '(I0)') visited_size

   ! We have to take a copy of visited, as we will call travel again, which
   ! modifies visited.
   allocate(visited_copy(visited_size))
   visited_copy = visited(1:visited_size)

   ! Part 2: put a wall in each visited position and check if it causes a loop
   do i = 2, visited_size  ! starting at 2 to skip guard position
      r = visited_copy(i)%r
      c = visited_copy(i)%c

      lab(r)(c:c) = '#'
      call travel(lab, guard, visited, visited_size, loops)
      lab(r)(c:c) = '.'

      if (loops) num_loops = num_loops + 1
   end do

   write(*, '(I0)') num_loops

end program main
