include "../vararray.f90"

program vararry_test
  use vararray
  implicit none

  integer,parameter :: dp = kind(0.d0)

  real(dp),  allocatable :: vec_real(:),mat_real(:,:)
  integer,  allocatable :: vec_integer(:),mat_integer(:,:)

  print*,"Vector real --------------------------------------------------"
  print*,"  Push 1.0"
  call push(vec_real,1._dp)
  print*,"  ",vec_real
  print*,"  Push (2.0,3.0)"
  call push(vec_real,(/2._dp,3._dp/))
  print*,"  ",vec_real
  
  print*,"Matrix real --------------------------------------------------"
  print*,"  Push (2.0,3.0)"
  call push(mat_real,(/2._dp,3._dp/))
  print*,"  ",mat_real
  print*,"  Push (1.0,7.0)"
  call push(mat_real,(/1._dp,7._dp/))
  print*,"  ",mat_real(1,:)
  print*,"  ",mat_real(2,:)

  print*,"Vector integer -----------------------------------------------"
  print*,"  Push 1"
  call push(vec_integer,1)
  print*,"  ",vec_integer
  print*,"  Push (2,3)"
  call push(vec_integer,(/2,3/))
  print*,"  ",vec_integer
  
  print*,"Matrix integer -----------------------------------------------"
  print*,"  Push (2,3)"
  call push(mat_integer,(/2,3/))
  print*,"  ",mat_integer
  print*,"  Push (1,7)"
  call push(mat_integer,(/1,7/))
  print*,"  ",mat_integer(1,:)
  print*,"  ",mat_integer(2,:)
end program vararry_test
