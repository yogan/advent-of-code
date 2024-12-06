program tests
   use aoc
   implicit none

   call test_find_guard()

contains

   subroutine test_find_guard()
      character(len=10), dimension(10) :: lab = &
            ['....#.....', &
             '.........#', &
             '..........', &
             '..#.......', &
             '.......#..', &
             '..........', &
             '.#..^.....', &
             '........#.', &
             '#.........', &
             '......#...']
      integer :: r, c, rows = 10, cols = 10

      call find_guard(lab, r, c)

      if (r /= 7 .or. c /= 5) then
         write(*, '(A, I0, A, I0)') &
            "Test failed (find_guard): expected 7/5, got ", r, "/", c
      else
         write(*, '(A)') "Test passed (find_guard)"
      end if
   end subroutine test_find_guard

end program tests
