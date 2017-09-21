subroutine sublat_ix(ix0,ix1,lat_in,lat_out)
  use bmad
  use lat_ele_loc_mod
  implicit none
  integer, intent(in) :: ix0,ix1
  type(lat_struct), target, intent(in) :: lat_in
  type(lat_struct), target, intent(inout) :: lat_out
  integer :: i
  type(ele_struct), pointer :: e0,e1

  lat_out = lat_in
  if (ix0.gt.0) lat_out%ele(1:ix0-1)%key = -1
  lat_out%ele(ix1+1:lat_in%n_ele_track)%key = -1
  call remove_eles_from_lat(lat_out)
  if (ix0.gt.0) then
     e0 => lat_out%ele(0)
     e1 => lat_in%ele(ix0-1)
     e0%value(E_tot$) = e1%value(E_tot$)
     e0%value(p0c$) = e1%value(p0c$)
     e0%a = e1%a
     e0%b = e1%b
     e0%z = e1%z
     e0%x = e1%x
     e0%y = e1%y
     e0%floor = e1%floor
     e0%map_ref_orb_in = e1%map_ref_orb_in
     e0%time_ref_orb_in = e1%time_ref_orb_in
     e0%s_start = 0.0_rp
     e0%ref_time = 0.0_rp
     e0%value(p0c_start$) = e1%value(p0c_start$)
     e0%value(e_tot_start$) = e1%value(e_tot_start$)
     call set_flags_for_changed_attribute(e0)
  end if
  do i=lat_out%n_ele_track+1,lat_out%n_ele_max
     e0 => lat_out%ele(i)
     if (e0%lord_status.eq.multipass_lord$.and.e0%key.eq.sbend$) then
        e0%value(n_ref_pass$) = 0
        call set_flags_for_changed_attribute(e0,e0%value(n_ref_pass$))
     end if
  end do
  call lattice_bookkeeper(lat_out)
  
end subroutine sublat_ix

subroutine sublat_loc(loc_str0,loc_str1,lat_in,lat_out)
  use bmad
  implicit none
  character(*), intent(in) :: loc_str0,loc_str1
  type(lat_struct), target, intent(in) :: lat_in
  type(lat_struct), target, intent(inout) :: lat_out
  interface
     subroutine sublat_ix(ix0,ix1,lat_in,lat_out)
       use bmad
       implicit none
       integer, intent(in) :: ix0,ix1
       type(lat_struct), target, intent(in) :: lat_in
       type(lat_struct), target, intent(inout) :: lat_out
     end subroutine sublat_ix
  end interface
  integer :: i0,i1,ne
  type(ele_pointer_struct), allocatable, dimension(:) :: el

  call lat_ele_locator(loc_str0,lat_in,el,ne)
  if (ne.ne.1) stop 'sublat_loc: loc_str0 not found'
  i0 = el(1)%loc%ix_ele
  call lat_ele_locator(loc_str1,lat_in,el,ne)
  if (ne.ne.1) stop 'sublat_loc: loc_str1 not found'
  i1 = el(1)%loc%ix_ele
  deallocate(el)

  call sublat_ix(i0,i1,lat_in,lat_out)

end subroutine sublat_loc
