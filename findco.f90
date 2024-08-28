subroutine findco2(period,co,m,failure)
  use bmad
  implicit none
  type(lat_struct), intent(inout) :: period
  type(coord_struct), intent(inout) :: co
  logical, intent(out), optional :: failure
  real(rp), dimension(6,6), intent(out) :: m
  integer :: i,j
  real(rp), dimension(6) :: z,z0
  logical :: err_flag
  integer :: ix,s,it
  logical, dimension(2) :: bounce
  real(rp), dimension(2) :: err0,err1
  real(rp) :: c1,k
  integer, dimension(2), parameter :: jp=(/2,1/)
  real(rp), dimension(2), parameter :: js=(/1.0_rp,-1.0_rp/)
  real(rp), dimension(2) :: tp,tm
  type(coord_struct), dimension(2) :: o

  s = 0
  it = 0
  bounce = (/(.false.,i=1,2)/)
  err0 = (/(huge(1d0),i=1,2)/)
  if (present(failure)) failure = .false.
  z = co%vec
  o(1) = co
  do ix=1,period%n_ele_track
     call make_mat6(period%ele(ix),period%param,o(1),o(2),err_flag)
     if (err_flag) then
        if (present(failure)) failure = .true.
        return
     end if
     o(1) = o(2)
  end do
  do while (.true.)
     err1 = o(2)%vec(1:2)-z(1:2)
     select case (s)
     case (0)
        ! Wait until errors have decreased simultaneously in all components
        if (it.ge.1.and.all(err1.eq.0.0_rp.or.abs(err1).lt.abs(err0)).or.it.ge.4) s = 1
     case (1)
        ! Quit once there's evidence of numerical noise in all components
        bounce = bounce.or.abs(err1).ge.abs(err0)
        if (all(bounce)) return
     end select
     err0 = err1
     it = it+1
     call transfer_matrix_calc(period,m)
     ! Compute (M-I)^{-1}(f(z)-z); assumption that M is symplectic
     c1 = 0.0_rp
     do i=1,2
        c1 = c1 + m(i,i)
     end do
     c1 = 0.5_rp/(2-c1)
     do i=1,2
        tm(i)=0.0_rp
        tp(i)=0.0_rp
        do j=1,2
           tp(i) = tp(i)+m(i,j)*err1(j)
           tm(i) = tm(i)+js(i)*js(j)*m(jp(j),jp(i))*err1(j)
        end do
     end do
     err1 = 0.5_rp*err1 + c1*(tp-tm)
     err_flag = .true.
     k = 1.0_rp
     tm = z(1:2)
     do while (err_flag)
        z(1:2) = tm+k*err1
        co%vec(1:2) = z(1:2)
        o(1) = co
        do ix=1,period%n_ele_track
           call make_mat6(period%ele(ix),period%param,o(1),o(2),err_flag)
           if (err_flag) exit
           o(1) = o(2)
        end do
        if (err_flag) it = 0
        k = 0.5_rp*k
     end do
  end do
end subroutine findco2

subroutine findco2_old(period,co,m,failure)
  use bmad
  implicit none
  type(lat_struct), intent(inout) :: period
  type(coord_struct), allocatable, dimension(:), intent(inout) :: co
  logical, intent(out), optional :: failure
  real(rp), dimension(6,6), intent(out) :: m
  integer :: i,j
  real(rp), dimension(6) :: z,z0
  logical :: err_flag
  integer :: s,it
  logical, dimension(2) :: bounce
  real(rp), dimension(2) :: err0,err1
  real(rp) :: c1,k
  integer, dimension(2), parameter :: jp=(/2,1/)
  real(rp), dimension(2), parameter :: js=(/1.0_rp,-1.0_rp/)
  real(rp), dimension(2) :: tp,tm

  s = 0
  it = 0
  bounce = (/(.false.,i=1,2)/)
  err0 = (/(huge(1d0),i=1,2)/)
  z = co(0)%vec
  call track_all(period,co,err_flag=err_flag)
  if (err_flag) then
     if (present(failure)) failure=.true.
     return
  else
     if (present(failure)) failure=.false.
  end if
  do while (.true.)
     err1 = co(period%n_ele_track)%vec(1:2)-z(1:2)
     select case (s)
     case (0)
        ! Wait until errors have decreased simultaneously in all components
        if (it.ge.1.and.all(err1.eq.0.0_rp.or.abs(err1).lt.abs(err0)).or.it.ge.4) s = 1
     case (1)
        ! Quit once there's evidence of numerical noise in all components
        bounce = bounce.or.abs(err1).ge.abs(err0)
        if (all(bounce)) return
     end select
     err0 = err1
     it = it+1
     call lat_make_mat6(period,ref_orb=co)
     call transfer_matrix_calc(period,m)
     ! Compute (M-I)^{-1}(f(z)-z); assumption that M is symplectic
     c1 = 0.0_rp
     do i=1,2
        c1 = c1 + m(i,i)
     end do
     c1 = 0.5_rp/(2-c1)
     do i=1,2
        tm(i)=0.0_rp
        tp(i)=0.0_rp
        do j=1,2
           tp(i) = tp(i)+m(i,j)*err1(j)
           tm(i) = tm(i)+js(i)*js(j)*m(jp(j),jp(i))*err1(j)
        end do
     end do
     err1 = 0.5_rp*err1 + c1*(tp-tm)
     err_flag = .true.
     k = 1.0_rp
     tm = z(1:2)
     do while (err_flag)
        z(1:2) = tm+k*err1
        call init_coord(co(0),z,period%ele(0),upstream_end$,co(0)%species)
        call track_all(period,co,err_flag=err_flag)
        if (err_flag) it = 0
        k = 0.5_rp*k
     end do
  end do
end subroutine findco2_old

subroutine findco4(period,co,m,failure)
  use bmad
  implicit none
  type(lat_struct), intent(inout) :: period
  type(coord_struct), allocatable, dimension(:), intent(inout) :: co
  real(rp), dimension(6,6), intent(out) :: m
  logical, intent(out), optional :: failure
  integer :: i,j,l
  real(rp), dimension(6) :: z
  logical :: err_flag
  integer :: s,it
  logical, dimension(4) :: bounce
  real(rp), dimension(4) :: err0,err1
  real(rp) :: c1,c2,k
  integer, dimension(4), parameter :: jp=(/2,1,4,3/)
  real(rp), dimension(4), parameter :: js=(/1.0_rp,-1.0_rp,1.0_rp,-1.0_rp/)
  real(rp), dimension(4) :: tp,tm

  z = co(0)%vec
  bounce = (/(.false.,i=1,4)/)
  err0 = (/(huge(1d0),i=1,4)/)
  s = 0
  it = 0
  call track_all(period,co,err_flag=err_flag)
  if (err_flag) then
     if (present(failure)) failure=.true.
     return
  else
     if (present(failure)) failure=.false.
  end if
  do while (.true.)
     err1 = co(period%n_ele_track)%vec(1:4)-z(1:4)
     select case (s)
     case (0)
        ! Wait until errors have decreased simultaneously in all components
        if (it.ge.1.and.all(err1.eq.0.0_rp.or.abs(err1).lt.abs(err0)).or.it.ge.4) s = 1
     case (1)
        ! Quit once there's evidence of numerical noise in all components
        bounce = bounce.or.abs(err1).ge.abs(err0)
        if (all(bounce)) return
     end select
     err0 = err1
     it = it+1
     call lat_make_mat6(period,ref_orb=co)
     call transfer_matrix_calc(period,m)
     ! Compute (M-I)^{-1}(f(z)-z); assumption that M is symplectic
     c1 = 0.0_rp
     c2 = 0.0_rp
     do i=1,4
        c1 = c1 + m(i,i)
        do j=i+1,4
           c2 = c2 + m(i,i)*m(j,j) - m(i,j)*m(j,i)
        end do
     end do
     c2 = 0.5_rp/(2-2*c1+c2)
     c1 = (2-c1)*c2
     do i=1,4
        tm(i)=0.0_rp
        tp(i)=0.0_rp
        do j=1,4
           tp(i) = tp(i)+m(i,j)*err1(j)
           tm(i) = tm(i)+js(i)*js(j)*m(jp(j),jp(i))*err1(j)
        end do
     end do
     err1 = 0.5_rp*err1 + c1*(tp-tm)
     do i=1,4
        do j=1,4
           err1(i) = err1(i) + c2*(m(i,j)*tp(j)-js(i)*js(j)*m(jp(j),jp(i))*tm(j))
        end do
     end do
     err_flag = .true.
     k = 1.0_rp
     tm = z(1:4)
     do while (err_flag)
        z(1:4) = tm+k*err1
        call init_coord(co(0),z,period%ele(0),upstream_end$,co(0)%species)
        call track_all(period,co,err_flag=err_flag)
        if (err_flag) it = 0
        k = 0.5_rp*k
     end do
  end do
end subroutine findco4

