! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Subroutine that solves the dynamic programmin problem of rural households.
!
! Data Dictionary:
!   File:   Export policy functions to "rural.txt".
!           xkpts  : current saving
!           optkr  : saving policy
!           optcar : food consumption
!           optcmr : manufacturing consumption
!           optcsr : service consumption
!           opthr  : formal labor supply
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
    subroutine rural
    
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
    real(dp) :: ca,cs,cm,hrval
    integer  :: ite,i,j,index
    
    real(dp), allocatable, dimension(:) :: mpivr,mpicar,mpicmr,mpicsr,mpihr,mpikr
	real(dp), allocatable, dimension(:) :: mpivs,mpicas,mpicms,mpicss,mpihs,mpiks
    external :: v_rural,const_rural,dv_rural,dconst_rural
    
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
    
    do j = 1,nzpts
    do i = 1,nkpts
       budget = (one-tauw)*wf*xhptsr(j) + &
                pa*xzptsr(j)*za*(smalld)**alphaa*(one-xhptsr(j))**(one-alphaa) + &
                pm*(one+re)*xkpts(i) + tr
       cs = psi*(budget - (one+taua)*pa*abar)/((one+psi+gamma)*ps)
       ca = ps*cs/(psi*(one+taua)*pa) + abar
       cm = gamma*cs*ps/(psi*(one+taum)*pm)
       
       if ( cs .le. zero ) cs = error
       if ( ca .le. zero ) ca = error
       if ( cm .le. zero ) cm = error
       
	   yr(i,j) = dlog(ca - abar) + psi*dlog( cs ) + gamma*dlog( cm )
	   
	   optkr (i,j) = xkpts(i)
	   optcsr(i,j) = cs
    enddo
    enddo

    ite = 1
    vmxdif = infty
    dmxdif = infty
    do while (ite .lt. mxiter .and. vmxdif .gt. error .and. dmxdif .gt. error)
                 
       y2r = yr
       optk2r = optkr
	   optcs2r = optcsr
	   vr = zero

	   do i = 1,nzpts
	      do j = 1,nzpts
	         vr(:,i) = vr(:,i)+pizr(i,j)*y2r(:,j)
	      enddo
	      call dpchim(nkpts,xkpts,vr(:,i),vpr(:,i),1,ierr)
	      
!	      if (ierr .ne. 0) print*, 'dpchim ierr =',ierr
	   enddo
  
       index = ipack+1
       do j = jend,jsta,-1
          do i = nkpts,1,-1
             
             index = index - 1
             xkcur = xkpts(i)
             nz = j
             xzcur = xzptsr(j)
             hrval = xhptsr(j)

	         bl(1) = xkpts(1)
	         bl(2) = 1.0d-04
	         
	         budget = (one-tauw)*wf*hrval + &
	                  pa*xzptsr(j)*za*(dr)**alphaa*(one-hrval)**(one-alphaa) + &
	                  pm*(one+re)*xkpts(i) + tr
	         bu(1) = dmin1(budget, xkpts(nkpts))
	         cs = psi*(budget - (one+taua)*pa*abar)/((one+psi+gamma)*ps)
	         bu(2) = cs
	         
	         x(1) = optk2r (i,j)
	         x(2) = optcs2r(i,j)
	         
	         call ffsqp(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,miter,inform, &
	                    bigbnd,eps2,epsneq,udelta,bl,bu,x,ff,g,iw,iwsize,work,nwsize,&
	                    v_rural,const_rural,dv_rural,dconst_rural)

             mpivs(index)  = -ff(1)
	         mpiks(index)  = x(1)
	         
			 cs = x(2)
			 ca = ps*cs/(psi*(one+taua)*pa) + abar
			 cm = gamma*cs*ps/(psi*(one+taum)*pm)

             mpicss(index) = cs
             mpicas(index) = ps*cs/(psi*(one+taua)*pa) + abar 
	         mpicms(index) = gamma*cs*ps/(psi*(one+taum)*pm)
	         mpihs(index)  = hrval   
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

       yr     = reshape(mpivr,  (/nkpts, nzpts/))
	   optkr  = reshape(mpikr,  (/nkpts, nzpts/))
	   optcsr = reshape(mpicsr, (/nkpts, nzpts/))
	   
	   optcar = reshape(mpicar, (/nkpts, nzpts/))
	   optcmr = reshape(mpicmr, (/nkpts, nzpts/))
	   opthr  = reshape(mpihr,  (/nkpts, nzpts/))

       vmxdif = maxval(dabs(y2r-yr))
       dmxdif = maxval(dabs(optk2r-optkr))       
       ite = ite + 1
    enddo
    
    if (myrank .eq. root) then
    open(1,file='rural.txt',form='formatted')
    do j = 1,nzpts
    do i = 1,nkpts
       write(1,102) xkpts(i),optkr(i,j),optcar(i,j),optcmr(i,j),optcsr(i,j),opthr(i,j)
    102   format(6f12.6,f15.6)
    enddo
    enddo
    close(1)
    endif
    deallocate(mpivr,mpicar,mpicmr,mpicsr,mpihr,mpikr, &
	           mpivs,mpicas,mpicms,mpicss,mpihs,mpiks)
    
    return
    end subroutine rural