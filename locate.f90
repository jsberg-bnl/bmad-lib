function ele_index(loc_str,lat) result(ix_ele)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str
  type(lat_struct), target, intent(in) :: lat
  integer :: ix_ele
  integer :: n_loc
  logical :: err
  type(ele_pointer_struct), dimension(:), allocatable :: eles

  call lat_ele_locator(loc_str,lat,eles,n_loc,err)
  if (err.or.n_loc.ne.1) then
     ix_ele = -1
  else
     ix_ele = eles(1)%loc%ix_ele
  end if
  if (allocated(eles)) deallocate(eles)
end function ele_index

function ele_pointer(loc_str,lat) result(ele_ptr)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str
  type(lat_struct), target, intent(in) :: lat
  type(ele_struct), pointer :: ele_ptr
  integer :: n_loc
  logical :: err
  type(ele_pointer_struct), dimension(:), allocatable :: eles
  
  call lat_ele_locator(loc_str,lat,eles,n_loc,err)
  if (err.or.n_loc.ne.1) then
     nullify(ele_ptr)
  else
     ele_ptr => eles(1)%ele
  end if
  if (allocated(eles)) deallocate(eles)
end function ele_pointer
