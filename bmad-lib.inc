  interface
     function ele_pointer(loc_str,lat) result(ele_ptr)
       use bmad
       character(*), intent(in) :: loc_str
       type(lat_struct), target, intent(in) :: lat
       type(ele_struct), pointer :: ele_ptr
     end function ele_pointer
     function ele_index(loc_str,lat) result(ix_ele)
       use bmad
       character(*), intent(in) :: loc_str
       type(lat_struct), target, intent(in) :: lat
       integer :: ix_ele
     end function ele_index
     subroutine findco4(period,co,m,failure)
       use bmad
       implicit none
       type(lat_struct), intent(inout) :: period
       type(coord_struct), allocatable, dimension(:), intent(inout) :: co
       real(rp), dimension(6,6), intent(out) :: m
       logical, intent(out), optional :: failure
     end subroutine findco4
     subroutine findco2(period,co,m,failure)
       use bmad
       implicit none
       type(lat_struct), intent(inout) :: period
       type(coord_struct), allocatable, dimension(:), intent(inout) :: co
       real(rp), dimension(6,6), intent(out) :: m
       logical, intent(out), optional :: failure
     end subroutine findco2
     subroutine write_attribute_real(u,ele,attrib_name)
       use bmad
       implicit none
       integer, intent(in) :: u
       type(ele_struct), intent(in) :: ele
       character(*), intent(in) :: attrib_name
     end subroutine write_attribute_real
     subroutine bmad_parse_from_argument(i,lat)
       use bmad
       implicit none
       integer, intent(in) :: i
       type(lat_struct), target, intent(inout) :: lat
     end subroutine bmad_parse_from_argument
     subroutine bmad_parse2_from_argument(i,lat)
       use bmad
       implicit none
       integer, intent(in) :: i
       type(lat_struct), target, intent(inout) :: lat
     end subroutine bmad_parse2_from_argument
  end interface
  interface sublat
     subroutine sublat_loc(loc_str0,loc_str1,lat_in,lat_out)
       use bmad
       implicit none
       character(*), intent(in) :: loc_str0,loc_str1
       type(lat_struct), target, intent(in) :: lat_in
       type(lat_struct), target, intent(inout) :: lat_out
     end subroutine sublat_loc
     subroutine sublat_ix(ix0,ix1,lat_in,lat_out)
       use bmad
       implicit none
       integer, intent(in) :: ix0,ix1
       type(lat_struct), target, intent(in) :: lat_in
       type(lat_struct), target, intent(inout) :: lat_out
     end subroutine sublat_ix
  end interface sublat
  interface set_attribute
     subroutine set_attribute_real_ele(ele,attrib_name,val,do_flag)
       use bmad
       type(ele_struct), target, intent(inout) :: ele
       character(*), intent(in) :: attrib_name
       real(rp), intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_real_ele
     subroutine set_attribute_value_ele(ele,value_index,val,do_flag)
       use bmad
       type(ele_struct), target, intent(inout) :: ele
       integer, intent(in) :: value_index
       real(rp), intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_value_ele
     subroutine set_attribute_real_loc(loc_str,lat,attrib_name,val,do_flag)
       use bmad
       type(lat_struct), target, intent(inout) :: lat
       character(*), intent(in) :: loc_str,attrib_name
       real(rp), intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_real_loc
     subroutine set_attribute_logical_ele(ele,attrib_name,val,do_flag)
       use bmad
       type(ele_struct), target, intent(inout) :: ele
       character(*), intent(in) :: attrib_name
       logical, intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_logical_ele
     subroutine set_attribute_logical_loc(loc_str,lat,attrib_name,val,do_flag)
       use bmad
       type(lat_struct), target, intent(inout) :: lat
       character(*), intent(in) :: loc_str,attrib_name
       logical, intent(in) :: val
       logical, intent(in), optional :: do_flag
     end subroutine set_attribute_logical_loc
  end interface set_attribute
  interface get_attribute_real
     function get_attribute_real_ele(ele,attrib_name) result(val)
       use bmad
       type(ele_struct), target, intent(in) :: ele
       character(*), intent(in) :: attrib_name
       real(rp) :: val
     end function get_attribute_real_ele
     function get_attribute_real_loc(loc_str,lat,attrib_name) result(val)
       use bmad
       character(*), intent(in) :: loc_str,attrib_name
       type(lat_struct), target, intent(in) :: lat
       real(rp) :: val
     end function get_attribute_real_loc
     function bmad_const(lat,name) result(v)
       use bmad
       type(lat_struct), target, intent(in) :: lat
       character(*), intent(in) :: name
       real(rp) :: v
     end function bmad_const
  end interface
