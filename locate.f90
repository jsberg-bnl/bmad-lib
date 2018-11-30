function ele_index(loc_str,lat) result(ix_ele)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str
  type(lat_struct), target, intent(in) :: lat
  integer :: ix_ele
  character(len(loc_str)) :: loc_up
  integer :: i

  loc_up = upcase(loc_str)
  do i=0,lat%n_ele_max
     if (loc_up.eq.lat%ele(i)%name) then
        ix_ele = i
        return
     end if
  end do
  ix_ele = -1
end function ele_index

function ele_pointer(loc_str,lat) result(ele_ptr)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str
  type(lat_struct), target, intent(in) :: lat
  type(ele_struct), pointer :: ele_ptr
  integer ix
  interface
     function ele_index(loc_str,lat) result(ix_ele)
       use bmad
       character(*), intent(in) :: loc_str
       type(lat_struct), target, intent(in) :: lat
       integer :: ix_ele
     end function ele_index
  end interface

  ix = ele_index(loc_str,lat)
  if (ix.eq.-1) then
     nullify(ele_ptr)
  else
     ele_ptr => lat%ele(ix)
  end if
end function ele_pointer
