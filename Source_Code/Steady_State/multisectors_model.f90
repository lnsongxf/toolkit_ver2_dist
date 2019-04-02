! -----------------------------------------------------------------------------
!                             PROGRAM DESCRIPTION
! -----------------------------------------------------------------------------
!   
! Purpose:
!     - Main function solving "The Distributional Implications of Fiscal
!   Consolidation in Developing Countries"
!  
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!  
! Record of Revisions:
!         Date:                 Description of Changes
!     ===========        =================================
!      12/26/2016:                 Legacy Code
!      05/17/2017         Now takes parameters from local files
!
! Compiler Used:
!   Intel(R) Visual Fortran Compiler XE 14.0.0.103 [IA-64]
!   Commercial Release: Intel Parallel Studio XE 2013
!   Integrated with Microsoft Visual Studio 2012
!
! Library Used:
!   Parallel: Intel(R) MPI Library 2017 Update 1 for Windows
!   Scientific: HSL Archive Library
!               Scilab Fortran Implementation
! =============================================================================

    program multisectors

    ! Load in Modules
    use params              ! Exogeneous Parameters
    use calibparams         ! Endogeneous Parameters
    use states              ! Environmental Variables
    use arrays              ! Auxilliary Variables
    use prices              ! General Equilibrium Prices
    use aggstats            ! Aggregate States of the Economy
    use zbrent_opt          ! 1D Root Finding using Brent's Method
    use mpiparams           ! Environmetal Variables for MPI
    implicit none
    
    integer, parameter :: n = 11    ! Number of Endogeneous Parameters
    integer, parameter :: m = n     ! Number of Moments
	real(dp), dimension(m) :: f     ! Distance between Model and Data
	real(dp), dimension(n) :: x     ! Parameters to be calibrated
	real(dp) :: h,dmax,acc          ! Aux.variables for invoking VA05AD
	integer  :: maxfun, iprint      ! Aux.variables for invoking VA05AD
	real(dp), dimension(3000) :: ww ! Aux.variable for invoking VA05AD
	real(dp) :: xkinc,btime,etime
	logical  :: calib
	integer  :: i,j

    ! Read Parameters from files
    real(dp), dimension(14) :: par
    real(dp), dimension(10) :: targets
    real(dp), dimension(2)  :: trfs
    real(dp), dimension(3)  :: infra
    
    integer, dimension(2) :: compu_accu
    integer :: flag
    
    include 'mpif.h'

! =============================================================================
! Initializes MPI Environment    
    call MPI_INIT(mpiierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,mpiierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,mpiierr)
    
    call cpu_time(btime)

 !   open(5,file='compu_matlab.txt',form='formatted')
	!read(5,114) compu_accu
	!114  format(I6)
	!close(5)
 !   
 !   nzpts = compu_accu(1)
 !   nk2pts = compu_accu(2)
    
    allocate(xkpts(nkpts), xk2pts(nk2pts), xzptsu(nzpts), xzptsr(nzpts), &
        pinvu(nzpts), pinvr(nzpts), zpts(nzpts), xzptsue(nzpts), &
        xhptsu(nzpts),xhptsr(nzpts))
    allocate(pizu(nzpts,nzpts),pizr(nzpts,nzpts))
    
! =============================================================================
! Generate Geometric Grid Points for Saving Capital
    xkpts(1) = 0.00015_dp
    xkinc = zero
    do j = 1,60
       xkinc = xkinc+0.0065_dp*dble(real(j))
       do i = (j-1)*8+2,j*8+1
          xkpts(i) = xkpts(i-1)+xkinc
       enddo
    enddo
    
! Generate Denser Linear Grid Points for Saving
    xk2pts(1) = xkpts(1)
    xkinc = (xkpts(nkpts)-xkpts(1))/dble(real(nk2pts-1))
    do i = 2,nk2pts
       xk2pts(i) = xk2pts(i-1)+xkinc
    enddo
    
! =============================================================================
! Distribute Workload to Different Cores
! Distribution is based on Idiosyncratic Productivity Shock 
!   discretized using Tauchen
    call mpirange(nzpts,nprocs,myrank,jsta,jend)
    ipack = nkpts*(jend-jsta+1)
    iget  = nkpts*nzpts

! =============================================================================
    ! =============================================================================
    open(1,file='flag.txt',form='formatted')
	read(1,111) flag
	111  format(I1)
	close(1)
    
    open(2,file='param_matlab.txt',form='formatted')
	read(2,'(F14.9)') par
	close(2)
    
    open(3,file='targets_matlab.txt',form='formatted')
	read(3,'(F14.9)') targets
	close(3)
    
    open(4,file='ep_matlab.txt',form='formatted')
	read(4,'(F14.9)') eprice
	close(4)
    
    open(5,file='transfer_matlab.txt',form='formatted')
	read(5,'(F14.9)') trfs
	close(5)
    
    open(6,file='infra_matlab.txt',form='formatted')
	read(6,'(F14.9)') infra
	close(6)    
    
    psi    = par(1)
    gamma  = par(2)
    sigmar = par(3)
    sigmau = par(4)
    taua   = par(5)
    taur   = par(6)
    tauw   = par(7)
    za     = par(8)
    zm     = par(9)
    z      = par(10)
    muu    = par(11)
    mur    = par(12)
    muf    = par(13)
    abar   = par(14)
    tu = trfs(1)
    tr = trfs(2)
    ! infra(1) rural, infra(2) urban
    pubk = infra(1)+infra(2)
    
    ! Calculate effect of public capital
    za = za*(dexp(infra(1)))**infra(3)
    zm = zm*(dexp(infra(2)))**infra(3)
    z = z*(dexp(infra(1)))**infra(3)
    
    if (flag == 0) then
        solveprices = .false.            ! Whether the general equilibrium needs to be
    else
        solveprices = .true.
    endif   
    calib = .false.
    
! Vector x is submitted to function calfun
! Order of elements matters
! In the case of calibration, x serves also as the initial guess.
    x(1)  = abar
    x(2)  = psi
    x(3)  = gamma
    x(4)  = sigmar
    x(5)  = sigmau
    x(6)  = taua
    x(7)  = taur
    x(8)  = tauw
    x(9)  = za
    x(10) = zm
    x(11) = z

! =============================================================================
    calipara = (/'Subsistence','Serv.Pref','Manu.Pref','Var.Rural',&
                 'Var.Urban','Tax.A','Tax.Corp','Tax.Inc',&
                 'Agri.Z','Manu.Z','Export.Z'/)
    calimoment = (/'PoorFood','Serv.C','Manu.C','R.Gini.C',&
                    'U.Gini.C','TAX/GDP','CIT/TAX','PIT/TAX',&
                    'Serv.Y','Manu.Y','Export.Y'/)
    calidata(1) = 0.00_dp 
    calidata(2:11) = targets
    
! =============================================================================
! If the model is re-calibrated, invoke va05ad to minimize the square root of 
!   model implied values with data moments.
! If the model is not calibrated, invoke calfun and the distance between model
!   and data are reported.
    if ( calib ) then
       h = 1.0d-5
       acc = 1.0d-6
       dmax = 1.5_dp
       maxfun = 1000
       if (myrank .eq. root) then
       ! Only the root processor prints intermediate results of va05ad.
          iprint = 2
       else
          iprint = 0
       endif
       !IF (myrank .eq. root) THEN
       !    WRITE (*,*) 'Start Calibrating the Model.'
       !END IF
       call va05ad(m,n,f,x,h,dmax,acc,maxfun,iprint,ww)
    else
       !IF (myrank .eq. root) THEN
       !    WRITE (*,*) 'Start Evaluating the Model.'
       !END IF 
       call calfun(m,n,f,x)
    endif

! =============================================================================
! Root processor prints general equilibrium prices to file
    if (myrank .eq. root) then   
       open(1,file='parameters.txt',form='formatted')
          write(1,100) 'ps',eprice(1)
          write(1,100) 'pa',eprice(2)
          write(1,100) 'w ',eprice(3)
          write(1,100) 'wf',eprice(4)
          write(1,100) 'r ',eprice(5)
       100   format(a12,f16.10)
       close(1)
          write(*,*) ''
          write(*,*) 'Equilibrium Prices:'
          write(*,107) 'ps',eprice(1)
          write(*,107) 'pa',eprice(2)
          write(*,107) 'w ',eprice(3)
          write(*,107) 'wf',eprice(4)
          write(*,107) 'r ',eprice(5)
107       format(a12,f16.10)
          
          write(*,*) ''
          write(*,*) 'Total Tax Revenue: ', vattax + personaltax + businesstax
          write(*,*) 'Net Tax:           ', aggtax
! Root processor prints calibrated parameters to file       
       open(1,file='calibparams.txt',form='formatted')
          do i = 1,n
             write(1,141) x(i)
          enddo
       141   format(f16.10)
       close(1)
    endif  

    call cpu_time(etime)

! Root processor prints running time of the code to file           
    if (myrank .eq. root) then
        write (*,*) ''
        WRITE (*,116) 'Time = ',INT((etime-btime)/60.0_8), ' mins ', &
            INT(MOD(etime-btime,60.0_8)), ' secs Elapsed'
116         format(A7,I3,A6,I3,A13)
    endif
    
    call MPI_FINALIZE(mpiierr)  
    end program multisectors
    
