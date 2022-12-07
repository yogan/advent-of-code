program main
   use day08

   implicit none
   integer :: i

   do i = 0, 5
      print '("double(", i0, ") = ", i02)', i, double(i)
   end do
end program main
