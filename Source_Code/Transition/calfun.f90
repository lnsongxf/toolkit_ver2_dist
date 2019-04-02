!===============================================================
!   banana is the objective function of equilibrium conditions
!===============================================================
	subroutine calfun(m,n,f,x)
	
	use params
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
	integer  :: i,j,ii,jj,maxfev,ml,mu,mode2,nprint,info,nfev,iflag
    integer  :: indi
	real(dp) :: epsfcn,factor,pinc,d
    real(dp), dimension(5) :: diff_lin
    external :: fcn

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!    solveprices = .true.
    !equilibrium = .true.
    !extend = .false.
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! VAT case    
    !! new ss
    !taua = 0.1045361532_dp !0.0513352712_dp
    !taum = taua
    !! old ss
    !taua(1) = 0.0645361532_dp
    !taum(1) = taua(1)
    !
    !taur = 0.1155349787_dp
    !taust = taur
    !taui = taur*0.365_dp
    !
    !tauw = 0.0555203613_dp
    
! CIT case
    !taua = 0.0645361532_dp
    !taum = taua    
    !
    !! new ss
    !taur = 0.2400349787_dp
    !taust = taur
    !taui = taur*0.365_dp    
    !! old ss
    !taur(1) = 0.1155349787_dp
    !taust(1) = taur(1)
    !taui(1) = taur(1)*0.365_dp        
    !
    !tauw = 0.0555203613_dp
    
! PIT case
    !taua = 0.0645361532_dp
    !taum = taua    
    !
    !taur = 0.1155349787_dp
    !taust = taur
    !taui = taur*0.365_dp        
    !
    !! new ss
    !tauw = 0.1515203613_dp    
    !! old ss
    !tauw(1) = 0.0555203613_dp  
    
!! VAT with rural transfer
!    ! new ss
!    taua = 0.1045361532_dp !0.0513352712_dp
!    taum = taua
!    ! old ss
!    taua(1) = 0.0645361532_dp
!    taum(1) = taua(1)
!    
!    taur = 0.1155349787_dp
!    taust = taur
!    taui = taur*0.365_dp
!    
!    tauw = 0.0555203613_dp
!    
!    tu = 0.0_dp
!!    tr = 0.0_dp
!    tr = 0.2502_dp
!    tr(1) = 0.0_dp
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    call tauchen(nzpts,0.0_dp,rhou,sigmau,2.5_dp,zpts,pizu)
    do i = 1,nzpts
       xzptsu(i) = dexp(zpts(i))
    enddo
    
    do i = 1,nzpts
       xzptsue(i) = dmin1(xzptsu(nzpts)**(one-xue)*xzptsu(i)**xue,one)
    enddo
    
    p0 = matmul(pizu,pizu)
    do i = 1,500
       p0 = matmul(p0,pizu)
    end do
    pinvu = p0(1,:) 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    call tauchen(nzpts,0.0_dp,rhor,sigmar,2.5_dp,zpts,pizr)
    do i = 1,nzpts
       xzptsr(i) = dexp(zpts(i))
    enddo 
    
    p0 = matmul(pizr,pizr)
    do i = 1,500
       p0 = matmul(p0,pizr)
    end do
    pinvr = p0(1,:)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    if ( equilibrium ) then
        if ( myrank .eq. root) then
            write (*,*) 'Evaluate at Equilibrium Path.'
        endif
        if ( extend ) then
            if ( myrank .eq. root) then
                write (*,'(A25, I3)') 'Extend Path Length from ', ntran_in
            endif
           ! use solution with smaller t as initial guess
           open(1,file='eprices_out.txt',form='formatted')
           do indi = 1,ntran_in
               read(1,918) (eprice_in(indi,j), j=1,5)
           enddo
           close(1)
           
           diff_lin = (eprice_in(ntran_in,:) - eprice_in(ntran_in-1,:)) &
                    /(dble(ntran-ntran_in+1))
           eprice(1:ntran_in-1,:) = eprice_in(1:ntran_in-1,:)
           
           do indi = ntran_in,ntran
               eprice(indi,:) = eprice(indi-1,:) + diff_lin
           enddo            
        else
            if ( myrank .eq. root) then
                write (*,*) 'Load existing path.'
            endif
           open(1,file='eprices_out.txt',form='formatted')
              do i = 1,ntran
                 read(1,918) (eprice(i,j), j=1,5)
              enddo
           918   format(5f16.10)
           close(1)
        endif
    else
        if ( myrank .eq. root) then
            write (*,*) 'Solve for equilibrium price.'
            write (*,'(A42,I4)') 'Convergence should start from iteration ', (ntran-2)*5+2
        endif
       if ( extend ) then
           if ( myrank .eq. root) then
                write (*,'(A25, I3)') 'Extend Path Length from ', ntran_in
           endif
           ! use solution with smaller t as initial guess
           open(1,file='eprices_out.txt',form='formatted')
           do indi = 1,ntran_in
               read(1,918) (eprice_in(indi,j), j=1,5)
           enddo
           close(1)
           
           diff_lin = (eprice_in(ntran_in,:) - eprice_in(ntran_in-1,:)) &
                    /(dble(ntran-ntran_in+1))
           eprice(1:ntran_in-1,:) = eprice_in(1:ntran_in-1,:)
           
           do indi = ntran_in,ntran
               eprice(indi,:) = eprice(indi-1,:) + diff_lin
           enddo
       else
            if ( myrank .eq. root) then
                write (*,*) 'Load existing path.'
            endif
           open(1,file='eprices_out.txt',form='formatted')
              do i = 1,ntran
                 read(1,918) (eprice(i,j), j=1,5)
              enddo
           close(1)
           ! Linear initial guess
        !   ! new SS
        !   ! VAT cases
        !   eprice(:,1) = 20.2107926451_dp
        !   eprice(:,2) = 17.8504562279_dp
        !   eprice(:,3) =  6.1392647747_dp
        !   eprice(:,4) =  3.7113439115_dp
        !   eprice(:,5) =  0.0075659625_dp           
        !   
        !   d = (eprice(1,3) - eprice(ntran,3))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,3) = eprice(j-1,3) - d
        !   enddo                  
           
           ! CIT cases
           !eprice(:,1) = 18.9770141173_dp
           !eprice(:,2) = 17.5021520823_dp
           !eprice(:,3) =  5.6450069885_dp
           !eprice(:,4) =  3.6443422292_dp
           !eprice(:,5) =  0.0074094669_dp
           
        !   d = (eprice(1,1) - eprice(ntran,1))/(dble(real(ntran-2)))
        !   do j = 2,ntran-1
        !      eprice(j,1) = eprice(j-1,1) - d
        !   enddo
        !   
        !   d = (eprice(1,2) - eprice(ntran,2))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,2) = eprice(j-1,2) - d
        !   enddo
        !   
        !   d = (eprice(1,3) - eprice(ntran,3))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,3) = eprice(j-1,3) - d
        !   enddo       
        !
        !   d = (eprice(1,4) - eprice(ntran,4))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,4) = eprice(j-1,4) - d
        !   enddo              
        
           ! PIT cases   
        !   eprice(:,1) = 18.4114863329_dp
        !   eprice(:,2) = 17.0683448913_dp
        !   eprice(:,3) =  5.9190898180_dp
        !   eprice(:,4) =  3.8064388998_dp
        !   eprice(:,5) =  0.0092699065_dp
        !
        !   d = (eprice(1,1) - eprice(ntran,1))/(dble(real(ntran-2)))
        !   do j = 2,ntran-1
        !      eprice(j,1) = eprice(j-1,1) - d
        !   enddo
        !   
        !   d = (eprice(1,2) - eprice(ntran,2))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,2) = eprice(j-1,2) - d
        !   enddo
        !   
        !   d = (eprice(1,3) - eprice(ntran,3))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,3) = eprice(j-1,3) - d
        !   enddo       

           ! new SS
           ! VAT with cash
           !eprice(:,1) = prices_ss2(1)
           !eprice(:,2) = prices_ss2(2)
           !eprice(:,3) = prices_ss2(3)
           !eprice(:,4) = prices_ss2(4)
           !eprice(:,5) = prices_ss2(5)
           
        !   d = (eprice(1,3) - eprice(ntran,3))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,3) = eprice(j-1,3) - d
        !   enddo                             
        !
        !   d = (eprice(1,5) - eprice(ntran,5))/(dble(real(ntran-2)))
	       !do j = 2,ntran-1
        !      eprice(j,5) = eprice(j-1,5) - d
        !   enddo                             
           
           
        ! old ss
        !eprice(1,1) = prices_ss1(1)
        !eprice(1,2) = prices_ss1(2)
        !eprice(1,3) = prices_ss1(3)
        !eprice(1,4) = prices_ss1(4)
        !eprice(1,5) = prices_ss1(5)
       endif
    endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    do j = 1,5
       do i = 2,ntran-1
          jj = (j-1)*(ntran-2)+i-1
          p(jj) = eprice(i,j)
!if (myrank .eq. root) print*, p(jj), jj
       enddo
    enddo

	!xtol_minpack = 1.0d-5
	maxfev = 10000
	ml = nval
	mu = nval
	epsfcn = 1.0d-12
	mode2 = 1
	factor = 100.0_dp
	nprint = 0
    
    if ( equilibrium ) then
        call fcn(nval,p,fvec,iflag)
       !call hybrd(fcn,nval,p,fvec,xtol,maxfev,ml,mu,epsfcn,diag,mode2,&
       !            factor,nprint,info,nfev,fjac,ldfjac,rr,lr,qtf,wa1,wa2,wa3,wa4)
    else
        if ( myrank .eq. root) then
            write (*,'(A18,ES9.1)') ' Tolerance Level = ', xtol_minpack
        endif
       ! call fcn(nval,p,fvec,iflag)
       call hybrd(fcn,nval,p,fvec,xtol_minpack,maxfev,ml,mu,epsfcn,diag,mode2,&
                   factor,nprint,info,nfev,fjac,ldfjac,rr,lr,qtf,wa1,wa2,wa3,wa4)       
    endif 
    eprice(2:ntran-1,:) = reshape(p,(/ntran-2,5/))

    if ( myrank .eq. root ) then
        call printoutfile
    endif
    
    return
    end subroutine calfun
!===============================================================