function bmad_const(lat,name) result(v)
  use bmad
  use iso_fortran_env
  type(lat_struct), target, intent(in) :: lat
  character(*), intent(in) :: name
  real(rp) :: v
  integer :: ix

  call match_word(name,lat%constant(:)%name,ix)
  if (ix.gt.0) then
     v = lat%constant(ix)%value
  else
     write(error_unit,*) 'bmad_const: constant '''//trim(name)//''' not found in lattice'
     error stop
  end if
end function bmad_const
