subroutine set_attribute_real_ele(ele,attrib_name,val,do_flag)
  use bmad
  implicit none
  type(ele_struct), target, intent(inout) :: ele
  character(*), intent(in) :: attrib_name
  real(rp), intent(in) :: val
  logical, intent(in), optional :: do_flag
  logical :: err_flag,do_flag_loc
  type(all_pointer_struct) :: a_ptr

  call pointer_to_attribute(ele,upcase(attrib_name),.false.,a_ptr,err_flag)
  a_ptr%r = val
  if (present(do_flag)) then
     do_flag_loc = do_flag
  else
     do_flag_loc = .true.
  end if
  if (do_flag_loc) call set_flags_for_changed_attribute(ele,a_ptr%r)
end subroutine set_attribute_real_ele

subroutine set_attribute_logical_ele(ele,attrib_name,val,do_flag)
  use bmad
  implicit none
  type(ele_struct), target, intent(inout) :: ele
  character(*), intent(in) :: attrib_name
  logical, intent(in) :: val
  logical, intent(in), optional :: do_flag
  logical :: err_flag,do_flag_loc
  type(all_pointer_struct) :: a_ptr

  call pointer_to_attribute(ele,upcase(attrib_name),.false.,a_ptr,err_flag)
  a_ptr%l = val
  if (present(do_flag)) then
     do_flag_loc = do_flag
  else
     do_flag_loc = .true.
  end if
  if (do_flag_loc) call set_flags_for_changed_attribute(ele,a_ptr%r)
end subroutine set_attribute_logical_ele

subroutine set_attribute_real_loc(loc_str,lat,attrib_name,val,do_flag)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str,attrib_name
  type(lat_struct), target, intent(inout) :: lat
  real(rp), intent(in) :: val
  logical, intent(in), optional :: do_flag
  type(ele_struct), pointer :: e
  interface
     function ele_pointer(loc_str,lat) result(ele_ptr)
       use bmad
       character(*), intent(in) :: loc_str
       type(lat_struct), target, intent(in) :: lat
       type(ele_struct), pointer :: ele_ptr
     end function ele_pointer
     subroutine set_attribute_real_ele(ele,attrib_name,val,do_flag)
       use bmad
       type(ele_struct), target, intent(inout) :: ele
       character(*), intent(in) :: attrib_name
       real(rp), intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_real_ele
  end interface
     
  e => ele_pointer(loc_str,lat)
  call set_attribute_real_ele(e,attrib_name,val,do_flag)
end subroutine set_attribute_real_loc

subroutine set_attribute_logical_loc(loc_str,lat,attrib_name,val,do_flag)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str,attrib_name
  type(lat_struct), target, intent(inout) :: lat
  logical, intent(in) :: val
  logical, intent(in), optional :: do_flag
  type(ele_struct), pointer :: e
  interface
     function ele_pointer(loc_str,lat) result(ele_ptr)
       use bmad
       character(*), intent(in) :: loc_str
       type(lat_struct), target, intent(in) :: lat
       type(ele_struct), pointer :: ele_ptr
     end function ele_pointer
     subroutine set_attribute_logical_ele(ele,attrib_name,val,do_flag)
       use bmad
       type(ele_struct), target, intent(inout) :: ele
       character(*), intent(in) :: attrib_name
       logical, intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_logical_ele
  end interface
     
  e => ele_pointer(loc_str,lat)
  call set_attribute_logical_ele(e,attrib_name,val,do_flag)
end subroutine set_attribute_logical_loc

function get_attribute_real_ele(ele,attrib_name) result(val)
  use bmad
  implicit none
  type(ele_struct), target, intent(in) :: ele
  character(*), intent(in) :: attrib_name
  real(rp) :: val
  logical :: err_flag
  type(all_pointer_struct) :: a_ptr

  call pointer_to_attribute(ele,upcase(attrib_name),.false.,a_ptr,err_flag)
  val = a_ptr%r
end function get_attribute_real_ele

function get_attribute_real_loc(loc_str,lat,attrib_name) result(val)
  use bmad
  character(*), intent(in) :: loc_str,attrib_name
  type(lat_struct), target, intent(in) :: lat
  real(rp) :: val
  interface
     function ele_pointer(loc_str,lat) result(ele_ptr)
       use bmad
       character(*), intent(in) :: loc_str
       type(lat_struct), target, intent(in) :: lat
       type(ele_struct), pointer :: ele_ptr
     end function ele_pointer
     function get_attribute_real_ele(ele,attrib_name) result(val)
       use bmad
       type(ele_struct), target, intent(in) :: ele
       character(*), intent(in) :: attrib_name
       real(rp) :: val
     end function get_attribute_real_ele
  end interface
  val = get_attribute_real_ele(ele_pointer(loc_str,lat),attrib_name)
end function get_attribute_real_loc

subroutine write_attribute_real(u,ele,attrib_name)
  use bmad
  implicit none
  interface
     function get_attribute_real_ele(ele,attrib_name) result(val)
       use bmad
       type(ele_struct), target, intent(in) :: ele
       character(*), intent(in) :: attrib_name
       real(rp) :: val
     end function get_attribute_real_ele
  end interface
  integer, intent(in) :: u
  type(ele_struct), intent(in) :: ele
  character(*), intent(in) :: attrib_name

  write(u,'(a,''['',a,''] = '',sp,es25.17e3)') upcase(trim(ele%name)),upcase(trim(attrib_name)),&
       get_attribute_real_ele(ele,attrib_name)
end subroutine write_attribute_real
