program main
   use aoc
   implicit none

   character(len=80) :: filename
   character(len=200), dimension(:), allocatable :: lab

   ! pre-allocated, input is 130x130, so this will be more than enough
   type(position), dimension(130*130) :: visited
   integer :: visited_size = 0

   call get_command_argument(1, filename)
   call read_file(filename, lab)

   call part1(lab, visited, visited_size)

   ! Part 1 is the number of unique positions visited
   write(*, '(I0)') visited_size

end program main
