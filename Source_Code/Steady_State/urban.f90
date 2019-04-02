! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Subroutine that solves the dynamic programmin problem of urban households.
!
! Data Dictionary:
!   File:   Export policy functions to "urban.txt".
!           xkpts  : current saving
!           optku  : saving
!           optcau : food consumption
!           optcmu : manufacturing consumption
!           optcsu : service consumption
!           opthu  : formal labor supply
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    03/26/2017                 Legacy Code
!
! ===============================================================================
    subroutine urban
    
    use params
    use calibparams
    use states
    use arrays
    use prices
    use mpiparams
    implicit none
    
    integer, dimension(iwsize):: iw
	real(dp), dimension(nwsize) :: work
	real(dp), dimension(nparam) :: bl,bu,x
	real(dp), dimension(nf) :: ff
	real(dp), dimension(nineq+neq+1) :: g
    integer  :: mode,iprint,miter,inform,ierr
	integer  :: maxfev,ml,mu,mode2,nprint
	real(dp) :: eps2,epsneq,udelta,bigbnd
	real(dp) :: xtol,epsfcn,factor
    real(dp) :: vmxdif,dmxdif
    real(dp) :: ca,cs,cm,huval
    integer  :: ite,i,j,index,jj
    
    real(dp), allocatable, dimension(:) :: mpivr,mpicar,mpicmr,mpicsr,mpihr,mpikr
	real(dp), allocatable, dimension(:) :: mpivs,mpicas,mpicms,mpicss,mpihs,mpiks
    ! External routines used in value function iteration
    external :: v_urban,const_urban,dv_urban,dconst_urban
    
    include 'mpif.h'
    
    allocate(mpivr(iget),mpicar(iget),mpicmr(iget),mpicsr(iget),mpihr(iget),mpikr(iget),&
             mpivs(iget),mpicas(iget),mpicms(iget),mpicss(iget),mpihs(iget),mpiks(iget))
    
    mode = 100
    iprint = 0
    miter = 500
    eps2 = 1.0d-08
    epsneq = 1.0d-12
    udelta = zero
	bigbnd = 1.0d+10
	xtol = 1.0d-12
	maxfev = 10000
	ml = nval
	mu = nval
	epsfcn = 1.0d-12
	mode2 = 1
	factor = 100.0_dp
	nprint = 0
	
    
! ===============================================================================
! Intitalize value function:
! - Assume that hh does not save in the last period
! ===============================================================================
	do j = 1,nzpts
    do i = 1,nkpts
    ! Labor supply decision is intratemporal and is not related to saving
       budget = (one-tauw)*xzptsu(j)*w*xhptsu(j) + pm*(one+re)*xkpts(i) + tu + &
                 xzptsue(j)*ps*zs*(one-xhptsu(j))**(one-alphas)
       ! Optimal consumption decision         
       cs = psi*(budget - (one+taua)*pa*abar)/((one+psi+gamma)*ps)
       ca = ps*cs/(psi*(one+taua)*pa) + abar
       cm = gamma*cs*ps/(psi*(one+taum)*pm)

       if ( cs .le. zero ) cs = error
       if ( ca .le. abar ) ca = abar + error
       if ( cm .le. zero ) cm = error
       
	   yu(i,j) = dlog(ca - abar) + psi*dlog( cs ) + gamma*dlog( cm )
   
	   optku (i,j) = xkpts(i)
	   optcsu(i,j) = cs
    enddo
    enddo
 
! ===============================================================================
! Value Function Iteration
! ===============================================================================
    ite = 1
    vmxdif = infty
    dmxdif = infty
    do while (ite .lt. mxiter .and. vmxdif .gt. error .and. dmxdif .gt. error)
                 
       y2u = yu
       optk2u = optku
	   optcs2u = optcsu
	   vu = zero
       
       do j = 1,nzpts
	      do i = 1,nkpts
	         do jj = 1,nzpts
! vu(i,j) is the continuation value if current shock is j and bt+1 = ki
	            vu(i,j) = vu(i,j)+pizu(j,jj)*y2u(i,jj)
	         enddo
          enddo
          ! Calculate the derivative at the Lagrangian points for the 
          ! Piecewise Cubit Hermite Interpolation
          ! Derivatives saved in vpu.
          call dpchim(nkpts,xkpts,vu(:,j),vpu(:,j),1,ierr)
	   enddo
       
       index = ipack+1
       do j = jend,jsta,-1
          do i = nkpts,1,-1
             
             index = index - 1
             xkcur = xkpts(i)
             nz = j
             xzcur = xzptsu(j)
             huval = xhptsu(j)
             
	         bl(1) = xkpts(1)
	         bl(2) = 1.0d-04
 
	         budget = (one-tauw)*xzptsu(j)*w*huval + pm*(one+re)*xkcur + tu + &
	                                xzptsue(j)*ps*zs*(one-huval)**(one-alphas)
	                                                           
	         bu(1) = dmin1(budget, xkpts(nkpts))
             cs = psi*(budget - (one+taua)*pa*abar)/((one+psi+gamma)*ps)
	         bu(2) = cs

	         x(1) = optk2u (i,j)
	         x(2) = optcs2u(i,j)
             ! Solve the DP problem using Feasible Sequential Quadratic Programming
	         call ffsqp(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,miter,inform, &
	                    bigbnd,eps2,epsneq,udelta,bl,bu,x,ff,g,iw,iwsize,work,nwsize,&
	                    v_urban,const_urban,dv_urban,dconst_urban)
                        
	         mpivs(index)  = -ff(1)
	         mpiks(index)  = x(1)
	         
	         cs = x(2)
             ca = ps*cs/(psi*(one+taua)*pa) + abar
             cm = gamma*cs*ps/(psi*(one+taum)*pm)

             mpicss(index) = cs
	         mpicas(index) = ps*cs/(psi*(one+taua)*pa) + abar
	         mpicms(index) = gamma*cs*ps/(psi*(one+taum)*pm)
	         mpihs(index)  = huval

	      enddo
       enddo
       
       call MPI_BARRIER(MPI_COMM_WORLD, ierr) 
       call MPI_ALLGATHER(mpivs,ipack,MPI_DOUBLE_PRECISION,mpivr,ipack, &
                                MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,mpiierr)
     
       call MPI_BARRIER(MPI_COMM_WORLD, ierr) 
       call MPI_ALLGATHER(mpiks,ipack,MPI_DOUBLE_PRECISION,mpikr,ipack, &
                                MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,mpiierr)

       call MPI_BARRIER(MPI_COMM_WORLD, ierr) 
       call MPI_ALLGATHER(mpicas,ipack,MPI_DOUBLE_PRECISION,mpicar,ipack, &
                                MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,mpiierr)
       
       call MPI_BARRIER(MPI_COMM_WORLD, ierr) 
       call MPI_ALLGATHER(mpicms,ipack,MPI_DOUBLE_PRECISION,mpicmr,ipack, &
                                MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,mpiierr)
       
       call MPI_BARRIER(MPI_COMM_WORLD, ierr) 
       call MPI_ALLGATHER(mpicss,ipack,MPI_DOUBLE_PRECISION,mpicsr,ipack, &
                                MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,mpiierr)                         
       
       call MPI_BARRIER(MPI_COMM_WORLD, ierr) 
       call MPI_ALLGATHER(mpihs,ipack,MPI_DOUBLE_PRECISION,mpihr,ipack, &
                                MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,mpiierr)                         

       yu     = reshape(mpivr,  (/nkpts, nzpts/))
	   optku  = reshape(mpikr,  (/nkpts, nzpts/))
	   optcsu = reshape(mpicsr, (/nkpts, nzpts/))
	   
	   optcau = reshape(mpicar, (/nkpts, nzpts/))
	   optcmu = reshape(mpicmr, (/nkpts, nzpts/))
	   opthu  = reshape(mpihr,  (/nkpts, nzpts/))
	         
	         
       vmxdif = maxval(dabs(y2u-yu))
       dmxdif = maxval(dabs(optk2u-optku))      
!print*, ite,vmxdif 
       ite = ite + 1

    enddo
    
    if (myrank .eq. root) then
    open(1,file='urban.txt',form='formatted')
    do j = 1,nzpts
    do i = 1,nkpts
       write(1,102) xkpts(i),optku(i,j),optcau(i,j),optcmu(i,j),optcsu(i,j),opthu(i,j)
    102   format(6f12.6,f15.6)
    enddo
    enddo
    close(1)
    endif

    deallocate(mpivr,mpicar,mpicmr,mpicsr,mpihr,mpikr, &
	           mpivs,mpicas,mpicms,mpicss,mpihs,mpiks)
    
    return
    end subroutine urban