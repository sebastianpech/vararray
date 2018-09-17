module vararray

  implicit none
  
  integer, parameter, private :: dp=kind(0.d0)
  
  real(dp), private, allocatable :: temp_real_dp(:), temp_real_dp_mat(:,:)
  integer , private, allocatable :: temp_integer(:), temp_integer_mat(:,:)

  
  interface push
     module procedure push_real_dp
     module procedure push_integer
     module procedure push_real_dp_mat
     module procedure push_integer_mat
     module procedure push_real_dp_batch
     module procedure push_integer_batch
  end interface push

contains
  
  subroutine push_real_dp_batch (arr,data)
    real(dp),allocatable, intent(inout) :: arr(:)
    real(dp), intent(in) :: data(:)
    integer :: info

    if (allocated(arr)) then
        allocate(temp_real_dp (size(arr)+size(data)), STAT=info)
        temp_real_dp(1:size(arr)) = arr
        temp_real_dp(size(arr)+1:size(arr)+size(data)) = data
        call move_alloc(temp_real_dp,arr)
     else
        allocate(arr (size(data)), STAT=info)
        arr = data
     endif
     
    return
  end subroutine push_real_dp_batch
  
  subroutine push_real_dp (arr,data)
    real(dp),allocatable, intent(inout) :: arr(:)
    real(dp), intent(in) :: data
    integer :: info

    if (allocated(arr)) then
        allocate(temp_real_dp (size(arr)+1), STAT=info)
        temp_real_dp(1:size(arr)) = arr
        temp_real_dp(size(arr)+1) = data
        call move_alloc(temp_real_dp,arr)
     else
        allocate(arr (1), STAT=info)
        arr(1) = data
     endif
     
    return
  end subroutine push_real_dp

  subroutine push_real_dp_mat (arr,data)
    real(dp),allocatable, intent(inout) :: arr(:,:)
    real(dp), intent(in) :: data(:)
    integer :: info

    if (allocated(arr)) then
        allocate(temp_real_dp_mat (size(arr,1)+1,size(arr,2)), STAT=info)
        temp_real_dp_mat(1:size(arr,1),:) = arr
        temp_real_dp_mat(size(arr,1)+1,:) = data
        call move_alloc(temp_real_dp_mat,arr)
     else
        allocate(arr (1,size(data)), STAT=info)
        arr(1,:) = data
     endif
     
    return
  end subroutine push_real_dp_mat
  
  subroutine push_integer (arr,data)
    integer,allocatable, intent(inout) :: arr(:)
    integer, intent(in) :: data
    integer :: info

    if (allocated(arr)) then
        allocate(temp_integer (size(arr)+1), STAT=info)
        temp_integer(1:size(arr)) = arr
        temp_integer(size(arr)+1) = data
        call move_alloc(temp_integer,arr)
     else
        allocate(arr (1), STAT=info)
        arr(1) = data
     endif
     
    return
  end subroutine push_integer

  subroutine push_integer_batch (arr,data)
    integer,allocatable, intent(inout) :: arr(:)
    integer, intent(in) :: data(:)
    integer :: info

    if (allocated(arr)) then
        allocate(temp_integer (size(arr)+size(data)), STAT=info)
        temp_integer(1:size(arr)) = arr
        temp_integer(size(arr)+1:size(arr)+size(data)) = data
        call move_alloc(temp_integer,arr)
     else
        allocate(arr (size(data)), STAT=info)
        arr = data
     endif
     
    return
  end subroutine push_integer_batch
  
  subroutine push_integer_mat (arr,data)
    integer,allocatable, intent(inout) :: arr(:,:)
    integer, intent(in) :: data(:)
    integer :: info

    if (allocated(arr)) then
        allocate(temp_integer_mat (size(arr,1)+1,size(arr,2)), STAT=info)
        temp_integer_mat(1:size(arr,1),:) = arr
        temp_integer_mat(size(arr,1)+1,:) = data
        call move_alloc(temp_integer_mat,arr)
     else
        allocate(arr (1,size(data)), STAT=info)
        arr(1,:) = data
     endif
     
    return
  end subroutine push_integer_mat
  
end module vararray
