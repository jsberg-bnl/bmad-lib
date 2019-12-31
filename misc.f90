subroutine bmad_parse_from_argument(i,lat)
  use bmad
  implicit none
  integer, intent(in) :: i
  type(lat_struct), target, intent(inout) :: lat
  integer :: n

  if (command_argument_count().lt.i) stop 'Missing argument'
  call get_command_argument(i,length=n)
  block
    character(n) :: f
    call get_command_argument(i,value=f)
    call bmad_parser(f,lat)
  end block
end subroutine bmad_parse_from_argument
