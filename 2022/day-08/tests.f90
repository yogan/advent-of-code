program tests
   use day08

   implicit none

   integer :: actual, expected

   actual = double(4)
   expected = 8
   if (actual /= expected) then
      print *, "Test failed: double(4) returned ", actual, &
         " instead of ", expected
   end if
end program tests
