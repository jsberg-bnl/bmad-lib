subroutine set_attribute_real(ele,attrib_name,val,do_flag)
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
end subroutine set_attribute_real

function get_attribute_real(ele,attrib_name) result(val)
  use bmad
  implicit none
  type(ele_struct), target, intent(in) :: ele
  character(*), intent(in) :: attrib_name
  real(rp) :: val
  logical :: err_flag
  type(all_pointer_struct) :: a_ptr

  call pointer_to_attribute(ele,upcase(attrib_name),.false.,a_ptr,err_flag)
  val = a_ptr%r
end function get_attribute_real
