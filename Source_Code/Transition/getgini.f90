!===============================================================
!  Calculate Gini Coefficient. 
!===============================================================
    subroutine getgini(fy,y,ny,gini)     
    
    implicit none
    
    integer,  parameter :: dp = kind(1.0d0)
    
    integer,  intent(in) :: ny
    real(dp), intent(in), dimension(ny) :: fy,y
    real(dp), intent(out) :: gini

    real(dp), parameter :: zero = 0.0_dp,one = 1.0_dp,error=1.0d-10
    real(dp), allocatable, dimension(:) :: lorenzx, lorenzy, div, w
    real(dp) :: lorenzval, lorenzyval, aggpop
    integer  :: i,ii,ihi 

    real(dp) :: QG01AD
	external :: QG01AD
	
    allocate(lorenzx(ny),lorenzy(ny),div(ny),w(5*ny))

    lorenzx = zero
    lorenzy = zero
    div   = zero
    
    aggpop = zero
	do i = 1,ny
	   aggpop = aggpop + fy(i)
	enddo
	
	ii = 0
	lorenzval = zero
    do i = 1,ny
       lorenzval = lorenzval + fy(i)/aggpop
       if (fy(i) .gt. error) then
          ii = ii + 1
          lorenzx(ii) = lorenzval
       endif

    enddo
    ihi = ii
    
    ii = 0
    lorenzval = zero
    do i = 1,ny
       lorenzval = lorenzval + fy(i)*y(i)/aggpop
       if (fy(i) .gt. error) then
          ii = ii + 1
          lorenzy(ii) = lorenzval
       endif
    enddo
       
    lorenzval = lorenzx(ihi)
    lorenzx = lorenzx/lorenzval
    lorenzval = lorenzy(ihi)
    lorenzy = lorenzy/lorenzval
    
    call tb04ad(ihi,lorenzx(1:ihi),lorenzy(1:ihi),div(1:ihi),w)
    lorenzyval = qg01ad(1,ihi,ihi,lorenzx(1:ihi),lorenzy(1:ihi),div(1:ihi))
    gini = 2.0_dp*(0.5_dp - lorenzyval)


    open(1,file='gini.txt',form='formatted')
    do i = 1,ihi
       write(1,102) lorenzx(i),lorenzy(i),div(i)
    102   format(6f12.6)
    enddo
    close(1)
 
    deallocate(lorenzx,lorenzy,div,w)

    return
    end subroutine getgini