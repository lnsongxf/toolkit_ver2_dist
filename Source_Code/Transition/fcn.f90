!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%  fcn  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine fcn(np,x,fvec,iflag)

    use params
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
    
    integer, intent(in) :: np,iflag
    real(dp), intent(in ), dimension(np) :: x
    real(dp), intent(out), dimension(np) :: fvec
    
    real(dp), dimension(ntran-2,5) :: x1
    real(dp) :: agghm,aggkcm,aggkpm,invtu,invtr,invtf
    integer :: i,nt1,j
    
    include 'mpif.h'
     
    call initialized_aggstats
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! equilibrium prices
    
    ps(1) = eprice(1,1)
    pa(1) = eprice(1,2)
    w(1)  = eprice(1,3)
    wf(1) = eprice(1,4)
    re(1) = eprice(1,5)
    
    ps(ntran) = eprice(ntran,1)
    pa(ntran) = eprice(ntran,2)
    w(ntran)  = eprice(ntran,3)
    wf(ntran) = eprice(ntran,4)
    re(ntran) = eprice(ntran,5)

    x1 = reshape(x,(/ntran-2,5/)) 
    ps(2:ntran-1) = x1(:,1)
    pa(2:ntran-1) = x1(:,2)
    w (2:ntran-1) = x1(:,3)
    wf(2:ntran-1) = x1(:,4)
    re(2:ntran-1) = x1(:,5)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%% Urban %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (myrank .eq. root) then
       allocate(trank(nkpts,nzpts,ntran),tranh(nkpts,nzpts,ntran), &
                trancs(nkpts,nzpts,ntran),trancm(nkpts,nzpts,ntran), &
                tranca(nkpts,nzpts,ntran),tranv(nkpts,nzpts,ntran))
       
       allocate(aggfc(nk2pts,ntran),aggfca(nk2pts,ntran), &
                  aggfk(nk2pts,ntran),aggfy(nk2pts,ntran))
       aggfc = zero
       aggfca = zero
       aggfk = zero
       aggfy = zero
    endif

    call urban

    if (myrank .eq. root) then
       allocate(distc(nk2pts,ntran),disty(nk2pts,ntran),distk(nk2pts,ntran),&
                               xc2pts(nk2pts),xca2pts(nk2pts),xy2pts(nk2pts))
       distc = zero
       disty = zero
       distk = zero
   
       call simulation_urban
       
       do nt = 1,ntran
          call getgini(distc(:,nt),xc2pts,nk2pts,ginicu(nt))
          call getgini(disty(:,nt),xy2pts,nk2pts,giniyu(nt))
          call getgini(distk(:,nt),xk2pts,nk2pts,giniku(nt))
          
          ! print*, 'gini',ginicu(nt),giniyu(nt)
       enddo
       
       deallocate(distc,disty,distk)
       deallocate(trank,tranh,trancs,trancm,tranca,tranv)
    endif
!%%% Rural %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (myrank .eq. root) then
       allocate(trank(nkpts,nzpts,ntran),tranh(nkpts,nzpts,ntran), &
                trancs(nkpts,nzpts,ntran),trancm(nkpts,nzpts,ntran), &
                tranca(nkpts,nzpts,ntran),tranv(nkpts,nzpts,ntran))
    endif
       
    call rural
    
    if (myrank .eq. root) then
       allocate(distc(nk2pts,ntran),disty(nk2pts,ntran),distk(nk2pts,ntran))
       distc = zero
       disty = zero
       distk = zero
       
       call simulation_rural
       
       do nt = 1,ntran
          call getgini(distc(:,nt),xc2pts,nk2pts,ginicr(nt))
          call getgini(disty(:,nt),xy2pts,nk2pts,giniyr(nt))
          call getgini(distk(:,nt),xk2pts,nk2pts,ginikr(nt))
       enddo
       
       deallocate(distc,disty,distk)
       deallocate(trank,tranh,trancs,trancm,tranca,tranv)
    endif

!%%% Farm %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (myrank .eq. root) call farm

!%%% Aggregate Statistics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (myrank .eq. root) then
    do nt = 1,ntran
       nt1 = max(1,nt-1)
       
       call getgini(aggfc(:,nt),xc2pts,nk2pts,ginict(nt))
       call getgini(aggfk(:,nt),xk2pts,nk2pts,ginikt(nt))
       call getgini(aggfy(:,nt),xy2pts,nk2pts,giniyt(nt))
       
       export(nt) = exfood(nt)
       dmfood(nt) = mur*rmfood(nt) + muf*za*(smalld**alphaa1)*(aggha(nt)**(one-alphaa1))
       
       aggkpm = muu*aggku(nt) + mur*aggkr(nt)
       if (nt .eq. 1) aggkcm = aggkpm
       
       agghm = muu*agghu(nt)
       manufacture(nt) = zm*(aggkcm**alpham)*(agghm**(one-alpham))
    
       output(nt) = pa(nt)*dmfood(nt) + exfood(nt) + &
                    ps(nt)*muu*service(nt) + pm(nt)*manufacture(nt)

       lowca(nt) = zero
       lowfca(nt) = zero
       i = 1
       do while ( lowfca(nt) .lt. 0.3_dp )
          lowca(nt) = lowca(nt) + aggfca(i,nt)*xca2pts(i)
          lowfca(nt) = lowfca(nt) + aggfca(i,nt)
          i = i + 1
       enddo
       
       do i = 1,nk2pts
          meanca(nt) = meanca(nt) + aggfca(i,nt)*xca2pts(i)
       enddo
              
       aggca(nt) = muu*aggcua(nt) + mur*aggcra(nt) + muf*aggcfa(nt)
       aggcm(nt) = muu*aggcum(nt) + mur*aggcrm(nt) + muf*aggcfm(nt)
       aggcs(nt) = muu*aggcus(nt) + mur*aggcrs(nt) + muf*aggcfs(nt)
!%%%%%%
       aggcu(nt) = pa(nt)*aggcua(nt) + pm(nt)*aggcum(nt) + ps(nt)*aggcus(nt)
       aggcr(nt) = pa(nt)*aggcra(nt) + pm(nt)*aggcrm(nt) + ps(nt)*aggcrs(nt)
       aggcf(nt) = pa(nt)*aggcfa(nt) + pm(nt)*aggcfm(nt) + ps(nt)*aggcfs(nt)
       
       aggcon(nt) = muu*aggcu(nt) + mur*aggcr(nt) + muf*aggcf(nt)
       
       invtu = aggku(nt) - (one-delta)*aggku(nt1)
       invtr = aggkr(nt) - (one-delta)*aggkr(nt1)
       invtf = aggkf(nt) - (one-delta)*aggkf(nt1)
       agginv(nt) = pm(nt)*(muu*invtu + mur*invtr + muf*invtf)
       
       personaltax(nt) = tauw(nt)*(muu*aggwu(nt) + mur*aggwr(nt))
       vattax(nt) = taua(nt)*pa(nt)*(muu*aggcua(nt) + mur*aggcra(nt)+ muf*aggcfa(nt)) + &
                    taum(nt)*pm(nt)*(muu*aggcum(nt) + mur*aggcrm(nt) + muf*aggcfm(nt))
       
       ! businesstax(nt) = businesstax(nt) + taui(nt)*pm(nt)*manufacture(nt)
       businesstax(nt) = muf*businesstax(nt) + taui(nt)*pm(nt)*manufacture(nt)
       aggtax(nt) = vattax(nt) + personaltax(nt) + businesstax(nt) - &
                                                (muu*tu(nt) + mur*tr(nt) + muf*tf)
       !aggtax(nt) = vattax(nt) + personaltax(nt) + muf*businesstax(nt) - &
       !                                         (muu*tu + mur*tr + muf*tf)
       gdp(nt) = aggcon(nt) + agginv(nt) + aggtax(nt)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

       if (nt .gt. 1 .and. nt .lt. ntran) then
          fvec(5*(nt-1)-4) = muu*aggcus(nt)+mur*aggcrs(nt)+muf*aggcfs(nt) - muu*service(nt)
          fvec(5*(nt-1)-3) = muu*aggcua(nt)+mur*aggcra(nt)+muf*aggcfa(nt) - dmfood(nt)
          fvec(5*(nt-1)-2) = (one-taui(nt))*pm(nt)*zm*(one-alpham)*(aggkcm**alpham)*(agghm**(-alpham)) - &
                                                                       w(nt)*aggeu(nt)                              
          fvec(5*(nt-1)-1) = mur*agghr(nt) - muf*(aggha(nt)+agghst(nt))
          fvec(5*(nt-1)) = (one-taui(nt))*pm(nt)*zm*alpham*(aggkcm**(alpham-one))*(agghm**(one-alpham)) &
                                                                       - (delta + re(nt))
       endif
       
       aggkcm = aggkpm
       
    enddo !end nt-loop
    
    deallocate(xc2pts, xca2pts, xy2pts)
    endif
    
    
    if (myrank .eq. root) then
       
       iteration = iteration + 1
       print*, iteration,'max. error on fvec ', maxval(dabs(fvec))
       
!       open(1,file='fnc_parameters.txt',form='formatted')
!          do i = 1,ntran
!             write(1,101) (eprice(i,j), j=1,5)
!          enddo
!       101   format(5f16.10)
!       close(1)
!       
!       open(1111,file='fnc_fvec.txt',form='formatted')
!          do i = 1,np
!             write(1111,102) fvec(i)
!          enddo
!       102   format(f16.10)
!       close(1111)
       
       
!       deallocate(trank,tranh,trancs,trancm,tranca)
       deallocate(aggfc,aggfca,aggfk,aggfy)
    endif
    
    call MPI_BARRIER(MPI_COMM_WORLD, mpiierr)
    call MPI_BCAST(fvec,np,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,mpiierr)
    
    return
    end subroutine fcn 