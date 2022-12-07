module day08
   implicit none
contains

   integer function double( x )
      integer, intent(in) :: x
      double = x * 2
   end function double

end module day08
