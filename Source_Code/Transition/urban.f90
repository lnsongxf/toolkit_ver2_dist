!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%  Urban Households %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine urban
    
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
    real(dp) :: ca,cs,cm,huval
    integer  :: ite,i,j,index,jj

    real(dp), allocatable, dimension(:) :: mpivr,mpicar,mpicmr,mpicsr,mpihr,mpikr
	real(dp), allocatable, dimension(:) :: mpivs,mpicas,mpicms,mpicss,mpihs,mpiks
    external :: v_urban,const_urban,dv_urban,dconst_urban
    
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
          huval = one - ((ps(nt)*zs*(one-alphas)*xzptsue(j))/ &
                         ((one-tauw(nt))*xzptsu(j)*w(nt)) )**(one/alphas)
          if (huval .lt. zero) huval = zero
          xhptsu(j) = huval
       enddo
       
       if ( nt .eq. 1 .or. nt .eq. ntran) then
	      do j = 1,nzpts
             do i = 1,nkpts
                budget = (one-tauw(nt))*xzptsu(j)*w(nt)*xhptsu(j) + &
                            ps(nt)*xzptsue(j)*zs*(one-xhptsu(j))**(one-alphas) + &
                                 pm(nt)*(one+re(nt))*xkpts(i) + tu(nt)
                cs = psi*(budget - (one+taua(nt))*pa(nt)*abar)/((one+psi+gamma)*ps(nt))
                ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
                cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))

                if ( cs .le. zero ) cs = error
                if ( ca .le. abar ) ca = abar + error
                if ( cm .le. zero ) cm = error
       
	            y(i,j) = dlog(ca - abar) + psi*dlog( cs ) + gamma*dlog( cm )
	            optk(i,j) = xkpts(i)
	            optcs(i,j) = cs
             enddo
          enddo
       endif
       
       
!     if (myrank .eq. root) then
!     open(1,file='V_urban.txt',form='formatted')
!     do j = 1,nzpts
!     do i = 1,nkpts
!        write(1,102) xkpts(i),y(i,j),optk(i,j),optcs(i,j)
!     enddo
!     enddo
!     close(1)
!     endif

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       ite = 1
       vmxdif = infty
       dmxdif = infty
       do while (ite .lt. mxiter .and. vmxdif .gt. error .and. dmxdif .gt. error)
                 
          y2 = y
          optk2 = optk
	      optcs2 = optcs
	      v = zero
       
          do j = 1,nzpts
	         do i = 1,nkpts
	            do jj = 1,nzpts
	               v(i,j) = v(i,j)+pizu(j,jj)*y2(i,jj)
	            enddo
             enddo
             call dpchim(nkpts,xkpts,v(:,j),vp(:,j),1,ierr)
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
 
	            budget = (one-tauw(nt))*xzptsu(j)*w(nt)*huval + &
	                      ps(nt)*xzptsue(j)*zs*(one-huval)**(one-alphas) + &
	                      pm(nt)*(one+re(nt))*xkcur + tu(nt)
	            bu(1) = dmin1(budget, xkpts(nkpts))

                if (budget - (one+taua(nt))*pa(nt)*abar .lt. zero) then
                   print'(3f12.6)', budget - (one+taua(nt))*pa(nt)*abar,budget,(one+taua(nt))*pa(nt)*abar
                   print'(4f12.6)', (one-tauw(nt))*xzptsu(j)*w(nt)*huval, &
                            ps(nt)*xzptsue(j)*zs*(one-huval)**(one-alphas), &
	                        pm(nt)*(one+re(nt))*xkcur, tu(nt)
                   print'(4f12.6)', w(nt),ps(nt),pm(nt),re(nt)
                   print*, i,j
                   stop
                endif
                
                cs = psi*(budget - (one+taua(nt))*pa(nt)*abar)/((one+psi+gamma)*ps(nt))
	            bu(2) = bu(1) !cs

	            x(1) = optk2 (i,j)
	            x(2) = optcs2(i,j)

	            call ffsqp(nparam,nf,nineqn,nineq,neqn,neq,mode,iprint,miter,inform, &
	                       bigbnd,eps2,epsneq,udelta,bl,bu,x,ff,g,iw,iwsize,work,nwsize,&
	                       v_urban,const_urban,dv_urban,dconst_urban)
                        
	            mpivs(index)  = -ff(1)
	            mpiks(index)  = x(1)
	         
	            cs = x(2)
                ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
                cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))

                mpicss(index) = cs
	            mpicas(index) = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
	            mpicms(index) = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))
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

          y     = reshape(mpivr,  (/nkpts, nzpts/))
	      optk  = reshape(mpikr,  (/nkpts, nzpts/))
	      optcs = reshape(mpicsr, (/nkpts, nzpts/))
	   
	      optca = reshape(mpicar, (/nkpts, nzpts/))
	      optcm = reshape(mpicmr, (/nkpts, nzpts/))
	      opth  = reshape(mpihr,  (/nkpts, nzpts/))
	         
	      ! jumps to the next iteration from ntran-1 to 2   
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
    !open(1,file='urban.txt',form='formatted')
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
    end subroutine urban