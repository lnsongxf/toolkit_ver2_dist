!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%  Rural Households %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine rural
    
    use params
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
	
	allocate (y2(nkpts,nzpts),optk2(nkpts,nzpts),v(nkpts,nzpts),&
              vp(nkpts,nzpts),y(nkpts,nzpts),optk(nkpts,nzpts),&
              opth(nkpts,nzpts),optcs2(nkpts,nzpts),optcs(nkpts,nzpts), &
              optca(nkpts,nzpts),optcm(nkpts,nzpts))

	allocate(mpivr(iget),mpicar(iget),mpicmr(iget),mpicsr(iget), &
	          mpihr(iget),mpikr(iget),mpivs(iget),mpicas(iget), &
	          mpicms(iget),mpicss(iget),mpihs(iget),mpiks(iget))
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!! Solve the model backward 
    do nt = ntran,1,-1
       
       do j = 1,nzpts
          hrval = one-((pa(nt)*xzptsr(j)*za*(dr**alphaa)*(one-alphaa))/ &
                                   ((one-tauw(nt))*wf(nt)) )**(one/alphaa)
          if (hrval .lt. zero) hrval = zero
          xhptsr(j) = hrval
       enddo
       
       if ( nt .eq. 1 .or. nt .eq. ntran) then
          do j = 1,nzpts
             do i = 1,nkpts
                budget = (one-tauw(nt))*wf(nt)*xhptsr(j) + &
                    pa(nt)*xzptsr(j)*za*(smalld)**alphaa*(one-xhptsr(j))**(one-alphaa) + &
                        pm(nt)*(one+re(nt))*xkpts(i) + tr(nt)
                cs = psi*(budget - (one+taua(nt))*pa(nt)*abar)/((one+psi+gamma)*ps(nt))
                ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
                cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))
       
                if ( cs .le. zero ) cs = error
                if ( ca .le. zero ) ca = error
                if ( cm .le. zero ) cm = error
       
	            y(i,j) = dlog(ca - abar) + psi*dlog( cs ) + gamma*dlog( cm )
	            optk (i,j) = xkpts(i)
	            optcs(i,j) = cs
             enddo
          enddo
       endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       ite = 1
       vmxdif = infty
       dmxdif = infty
       do while (ite .lt. mxiter .and. vmxdif .gt. error .and. dmxdif .gt. error)
                 
          y2 = y
          optk2 = optk
	      optcs2 = optcs
	      v = zero

	      do i = 1,nzpts
	         do j = 1,nzpts
	            v(:,i) = v(:,i)+pizr(i,j)*y2(:,j)
	         enddo
	         call dpchim(nkpts,xkpts,v(:,i),vp(:,i),1,ierr)
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

	         
	            budget = (one-tauw(nt))*wf(nt)*hrval + &
	                     pa(nt)*xzptsr(j)*za*(dr)**alphaa*(one-hrval)**(one-alphaa) + &
	                     pm(nt)*(one+re(nt))*xkpts(i) + tr(nt)
	            bu(1) = dmin1(budget, xkpts(nkpts))
	            cs = psi*(budget - (one+taua(nt))*pa(nt)*abar)/((one+psi+gamma)*ps(nt))
	            bu(2) = cs

if (budget - (one+taua(nt))*pa(nt)*abar .lt. zero) then
   print'(3f12.6)', budget - (one+taua(nt))*pa(nt)*abar,budget,(one+taua(nt))*pa(nt)*abar
   print'(4f12.6)', (one-tauw(nt))*wf(nt)*hrval, &
	                pa(nt)*xzptsr(j)*za*(dr)**alphaa*(one-hrval)**(one-alphaa), &
	                pm(nt)*(one+re(nt))*xkpts(i), tr(nt)
   print'(4f12.6)', wf(nt),ps(nt),pm(nt),re(nt)
   print*, i,j
   stop
endif
	         
	            x(1) = optk2 (i,j)
	            x(2) = optcs2(i,j)
	         
	            call ffsqp(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,miter,inform, &
	                       bigbnd,eps2,epsneq,udelta,bl,bu,x,ff,g,iw,iwsize,work,nwsize,&
	                       v_rural,const_rural,dv_rural,dconst_rural)

                mpivs(index)  = -ff(1)
	            mpiks(index)  = x(1)
	         
			    cs = x(2)
			    ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
			    cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))

                mpicss(index) = cs
                mpicas(index) = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar 
	            mpicms(index) = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))
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

          y     = reshape(mpivr,  (/nkpts, nzpts/))
	      optk  = reshape(mpikr,  (/nkpts, nzpts/))
	      optcs = reshape(mpicsr, (/nkpts, nzpts/))
	   
	      optca = reshape(mpicar, (/nkpts, nzpts/))
	      optcm = reshape(mpicmr, (/nkpts, nzpts/))
	      opth  = reshape(mpihr,  (/nkpts, nzpts/))

          vmxdif = zero
          dmxdif = zero  
          ite = ite + 1
          if (nt .eq. 1 .or. nt .eq. ntran) then
             vmxdif = maxval(dabs(y2-y))
             dmxdif = maxval(dabs(optk2-optk))
          endif
       enddo!!! end do while loop
       
       if (myrank .eq. root) then
          trank(:,:,nt) = optk
          tranh(:,:,nt) = opth
          trancs(:,:,nt) = optcs
          trancm(:,:,nt) = optcm
          tranca(:,:,nt) = optca
          tranv(:,:,nt) = y
       endif
    enddo !!! end nt loop
    
    !if (myrank .eq. root) then
    !open(1,file='rural.txt',form='formatted')
    !do j = 1,nzpts
    !do i = 1,nkpts
    !   write(1,102) xkpts(i),optk(i,j),optca(i,j),optcm(i,j),optcs(i,j),opth(i,j)
    !102   format(6f12.6,f15.6)
    !enddo
    !enddo
    !close(1)
    !endif
    deallocate(mpivr,mpicar,mpicmr,mpicsr,mpihr,mpikr, &
	           mpivs,mpicas,mpicms,mpicss,mpihs,mpiks)
    
    deallocate(y2,optk2,v,vp,y,optk,opth,optcs2,optcs,optca,optcm)

    return
    end subroutine rural