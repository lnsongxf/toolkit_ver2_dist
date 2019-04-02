! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Subroutine that solves the equilibrium of the model.
!   Auxilliary subroutine used for communicating with VA05AD.
!
! Data Dictionary:
!   Input:  m: number of moments.
!           n: number of variables.
!   Output: f: weighted distance between model and data
!           x: value of calibrated parameters
!   File: Distance between the model and the data for each target is exported
!           to "match.txt".
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    12/26/2016                 Legacy Code
!
! ===============================================================================
	subroutine calfun(m,n,f,x)
	
! Load Modules
	use params
    use calibparams
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
	implicit none
    
    integer :: m,n
    real(dp), dimension(n) :: x
    real(dp), dimension(m) :: f
    
    real(dp), parameter :: xue = 0.95_dp
    real(dp), dimension(nzpts,nzpts) :: p0
    real(dp), dimension(nval) :: p,fvec,diag,qtf,wa1,wa2,wa3,wa4
	real(dp), dimension(ldfjac,nval) :: fjac
    real(dp), dimension(lr) :: rr
	integer  :: i,maxfev,ml,mu,mode2,nprint,info,nfev,iflag
	real(dp) :: xtol,epsfcn,factor,xmin
    external :: fcn
    
    include 'mpif.h'

! ===============================================================================
! Check if any of the calibrated parameters are negative during the computation.
!   If yes, skip all the computation and export all distance as 1e50 to "match.txt".
    xmin = error
    do i = 1,n
       if (x(i) .lt. xmin) xmin = x(i)
    enddo
    
    if (xmin .le. zero ) then
       sumsqfunc = 1.0d+50
       goto 11
    endif

    abar   = x(1)
    psi    = x(2)
    gamma  = x(3)
    sigmar = x(4)
    sigmau = x(5)
    taua   = x(6)
    taum   = taua
    taur   = x(7)
    taust  = taur
    taui   = taur*0.365_dp
    tauw   = x(8)
    za     = x(9)
    zm     = x(10)
    z      = x(11)

! ===============================================================================
! Discretize the urban income shock using Tauchen.
! xzptsu is the efficiency of working in the urban market (manufacturing)
! xzptsue is the efficiency of self-employed in urban (service)
    call tauchen(nzpts,0.0_dp,rhou,sigmau,2.5_dp,zpts,pizu)
    do i = 1,nzpts
       xzptsu(i) = dexp(zpts(i))
    enddo
    
! Generate the urban informal productivity. 
! Algorithm: 1. Calculate geometric average of urban formal productivity with
!   maximum formal productivity. This will inflate the overall overall 
!   productivity.
!            2. The infltaed productivity is then taken minimum with 1.
! This generates the pattern that low productivity urban work more in informal.
    do i = 1,nzpts
       xzptsue(i) = dmin1(xzptsu(nzpts)**(one-xue)*xzptsu(i)**xue,one)
    enddo

! Calculate the stationary distribution    
    p0 = matmul(pizu,pizu)
    do i = 1,500
       p0 = matmul(p0,pizu)
    end do
    pinvu = p0(1,:)

! ===============================================================================
! Discretize the rural income shock using Tauchen.
! xzptsr is the efficiency of self-employed in the rural market (agricultural)
! Productivity of working in farm is normalized to one.
! This generates the pattern that low productivity rural work more in own farm.
    call tauchen(nzpts,0.0_dp,rhor,sigmar,2.5_dp,zpts,pizr)
    do i = 1,nzpts
       xzptsr(i) = dexp(zpts(i))
    enddo 

! Calculate the stationary distribution
    p0 = matmul(pizr,pizr)
    do i = 1,500
       p0 = matmul(p0,pizr)
    end do
    pinvr = p0(1,:)   

!if (myrank .eq. zero) then
!do i = 1,nzpts
!print*, i,pinvr(i),pinvu(i)
!enddo
!endif
!stop

! ===============================================================================
! Accuracy for solving equilibrium prices
    xtol = 1.0d-4
    epsfcn = 1.0d-7
	!xtol = 1.0d-5
    !epsfcn = 1.0d-12
	maxfev = 10000
	ml = nval
	mu = nval
	mode2 = 1
	factor = 100.0_dp
	nprint = 0
	
	p = eprice
    
! ===============================================================================
! - If solveprices == true, then solve the market clearing conditions.
!   Subroutine "hybrd" solves a system of equations using the Powell hybrid method.
!       The system of equations is supplied with subroutine "fcn".
!       The market clearing conditions are stored in vector "fvec".
!       The distance between model and data are stored in vector "sumsqfunc".
! - If solveprices == false, the distance between model and data are calculated
!   and stored in vector "sumsqfunc".
    if ( solveprices ) then
       if (myrank .eq. root) then 
          WRITE (*,225) 'Start solving general equilibrium with tol', xtol
225          format(A45,F7.4)
       endif        
       call hybrd(fcn,nval,p,fvec,xtol,maxfev,ml,mu,epsfcn,diag,mode2,&
                   factor,nprint,info,nfev,fjac,ldfjac,rr,lr,qtf,wa1,wa2,wa3,wa4)
    else
       if (myrank .eq. root) then 
          WRITE (*,*) 'Taken price as given.'
       endif        
       call fcn(nval,p,fvec,iflag)
    endif   
    eprice = p
    
11	do i = 1,m
       f(i) = sumsqfunc(i)*weight(i)
    enddo

    if (myrank .eq. root) then
       open(1,file='match.txt',form='formatted')
          WRITE (*,*) ''
          write (*,*) 'Model Fit:'
          WRITE (*,117) 'Parameter        ','Moments         ',&
                        'Para.Values','Errors','Model ','Data  '
          do i = 2,n
             write(1,141) x(i),sumsqfunc(i)
             write(*,116) calipara(i), calimoment(i), &
                          x(i),sumsqfunc(i), calimodel(i), calidata(i)
          enddo
          WRITE (*,116) 'Agri.Z          ', 'Agri.Y          ', x(9), &
            (1-calimodel(9)-calimodel(10)-calimodel(11))&
            -(1-calidata(9)-calidata(10)-calidata(11)),&
            1-calimodel(9)-calimodel(10)-calimodel(11), &
            1-calidata(9)-calidata(10)-calidata(11)
          141       format(2f16.10)
          116       FORMAT(2A16,2f10.4,2F10.4)   
          117       FORMAT(2A16,A11,A10,2A10)             
       close(1)
    endif

    return
    end subroutine calfun