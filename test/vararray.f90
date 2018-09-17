include "vararry.f90"

program vararry_test
  use vararray
  implicit none

  integer,parameter :: dp = kind(0.d0)

  real(dp),  allocatable, dimension(:) :: a

  allocate(a (10))

  print*,"asdf",size(a)

end program vararry_test
