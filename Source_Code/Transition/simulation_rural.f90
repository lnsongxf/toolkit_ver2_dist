!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine simulation_rural
    
    use params
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
     
    integer, parameter :: maxit = 100000
    real(dp), allocatable, dimension(:,:) :: f2,f,jks,wgts
    real(dp), allocatable, dimension(:,:) :: skp,sca,scs,scm,sh,sv
    integer :: i,j,k,ite,jk
    real(dp) :: wgt,fmxdif,fsum,income,ca
    real(dp) :: totc,toty
    
    allocate(skp(nk2pts,nzpts),sca(nk2pts,nzpts), &
             scs(nk2pts,nzpts),scm(nk2pts,nzpts), &
             sh(nk2pts,nzpts),f2(nk2pts,nzpts), &
             f(nk2pts,nzpts),jks(nk2pts,nzpts), &
             wgts(nk2pts,nzpts),sv(nk2pts,nzpts))
!!! Simulate the model forward   
    do nt = 1,ntran
       jk = 1
       do i = 1,nk2pts
          call hunt(xkpts,nkpts,xk2pts(i),jk)
          do j = 1,nzpts
             if (jk .lt. 1) then
                jk = 1
                wgt = one
             elseif (jk .ge. nkpts) then
                jk = nkpts-1
                wgt = zero
             else
                wgt = one-(xk2pts(i)-xkpts(jk))/(xkpts(jk+1)-xkpts(jk))
             endif

             skp(i,j) = wgt*trank(jk,j,nt) + (one-wgt)*trank(jk+1,j,nt)
             sca(i,j) = wgt*tranca(jk,j,nt) + (one-wgt)*tranca(jk+1,j,nt)
             scs(i,j) = wgt*trancs(jk,j,nt) + (one-wgt)*trancs(jk+1,j,nt)
             scm(i,j) = wgt*trancm(jk,j,nt) + (one-wgt)*trancm(jk+1,j,nt)
             sh(i,j) = wgt*tranh(jk,j,nt) + (one-wgt)*tranh(jk+1,j,nt)
             sv(i,j) = wgt*tranv(jk,j,nt) + (one-wgt)*tranv(jk+1,j,nt)
          enddo
       enddo

       do j = 1,nzpts
          jk = 1
          do i = 1,nk2pts
             call hunt(xk2pts,nk2pts,skp(i,j),jk)
             if (jk .lt. 1) then
                jk = 1
                wgt = one
             elseif (jk .ge. nk2pts) then
                jk = nk2pts-1
                wgt = zero
             else
                wgt = one-(skp(i,j)-xk2pts(jk))/(xk2pts(jk+1)-xk2pts(jk))
             endif
             jks(i,j) = jk
             wgts(i,j) = wgt
          enddo
       enddo       

       if (nt .eq. 1 .or. nt .eq. ntran) then
          f = zero
          do i = 1,nk2pts
             f(i,:) = pinvr(:)/dble(real(nk2pts))
          enddo
          
          ite = 1
          fmxdif = infty   
          do while ( ite .le. maxit .and. fmxdif .gt. error) 
             f2 = f
             f = zero
             do j = 1,nzpts
                do i = 1,nk2pts
                   jk = jks(i,j)
                   wgt = wgts(i,j)
                   do k = 1,nzpts
                      f(jk,k) = pizr(j,k)*wgt*f2(i,j)+f(jk,k)
                      f(jk+1,k) = pizr(j,k)*(one-wgt)*f2(i,j)+f(jk+1,k)
                   enddo
                enddo
             enddo
          
             fsum = sum(f)
             fmxdif = zero
             if ( nt .eq. 1 .or. nt .eq. ntran) fmxdif = maxval(dabs(f-f2))
             if (myrank .eq. root .and. dabs(fsum-one) > error) print*, 'failed rural'
             ite = ite+1
          enddo !!! end d0-while loop
          if (myrank .eq. root .and. nt .eq. ntran) then
              write (*,*) 'rural dist error nt   = ', maxval(dabs(f-f2))
          endif                                        
       else
          f2 = f
          f = zero
          do j = 1,nzpts
             do i = 1,nk2pts
                jk = jks(i,j)
                wgt = wgts(i,j)
                do k = 1,nzpts
                   f(jk,k) = pizr(j,k)*wgt*f2(i,j)+f(jk,k)
                   f(jk+1,k) = pizr(j,k)*(one-wgt)*f2(i,j)+f(jk+1,k)
                enddo
             enddo
          enddo
          if (myrank .eq. root .and. nt .eq. ntran-1) then
              write (*,*) 'rural dist error nt-1 = ', maxval(dabs(f-f2))
          endif          
       endif

if (equilibrium .eq. .true.) then              
     if (myrank .eq. root .and. nt .eq. 1) then   
     open(1,file='dist_rural_ss1.txt',form='formatted')
     do j =1,nzpts
        do i = 1,nk2pts
           write(1,100) xk2pts(i),f(i,j),sca(i,j),scs(i,j),scm(i,j),sh(i,j),skp(i,j),sv(i,j)
        100   format(12F16.9)
        enddo
     enddo
     close(1)     
     endif
     
     if (myrank .eq. root .and. nt .eq. 2) then   
     open(1,file='dist_rural_nt1.txt',form='formatted')
     do j =1,nzpts
        do i = 1,nk2pts
           write(1,100) xk2pts(i),f(i,j),sca(i,j),scs(i,j),scm(i,j),sh(i,j),skp(i,j),sv(i,j)
!        100   format(12f12.6)
        enddo
     enddo
     close(1)
     endif
     
     if (myrank .eq. root .and. nt .eq. ntran) then   
     open(1,file='dist_rural_ss2.txt',form='formatted')
     do j =1,nzpts
        do i = 1,nk2pts
           write(1,100) xk2pts(i),f(i,j),sca(i,j),scs(i,j),scm(i,j),sh(i,j),skp(i,j),sv(i,j)
!        100   format(12f12.6)
        enddo
     enddo
     close(1)
     endif     
endif

!     if (myrank .eq. root .and. nt.eq.1) then   
!     open(1,file='dist_rural.txt',form='formatted')
!     do j=1,nzpts
!        do i = 1,nk2pts
!           write(1,100) xk2pts(i),f(i,j),sca(i,j),scs(i,j),scm(i,j),sh(i,j),skp(i,j)
!        100   format(12f12.6)
!        enddo
!     enddo
!     endif
    
 !!!compute k,h,ca,cm,cs,distributions
       income = zero
       do j = 1,nzpts
          jk = 1
          do i = 1,nk2pts
             aggcra(nt) = aggcra(nt) + sca(i,j)*f(i,j)
             aggcrs(nt) = aggcrs(nt) + scs(i,j)*f(i,j)
             aggcrm(nt) = aggcrm(nt) + scm(i,j)*f(i,j)
             agghr(nt)  = agghr(nt)  + sh(i,j)*f(i,j)
             aggkr(nt)  = aggkr(nt)  + skp(i,j)*f(i,j)
             aggwr(nt)  = aggwr(nt)  + wf(nt)*sh(i,j)*f(i,j)
          
             totc = pa(nt)*sca(i,j)+ps(nt)*scs(i,j)+pm(nt)*scm(i,j)
             call hunt(xc2pts,nk2pts,totc,jk)
             if (jk .lt. 1) then
                jk = 1
                wgt = one
             elseif (jk .ge. nk2pts) then
                jk = nk2pts-1
                wgt = zero
             else
                wgt = one-(totc-xc2pts(jk))/(xc2pts(jk+1)-xc2pts(jk))
             endif
             distc(jk,nt)   = distc(jk  ,nt)   +    wgt *f(i,j)
             distc(jk+1,nt) = distc(jk+1,nt) + (one-wgt)*f(i,j)
             
             distk(i,nt) = distk(i,nt) + f(i,j)
          enddo
       enddo
!!!!!!!    
       do j = 1,nzpts
          jk = 1
          do i = 1,nk2pts
             toty = (one-tauw(nt))*wf(nt)*sh(i,j) + pm(nt)*re(nt)*xk2pts(i) + tr(nt) + &
                     pa(nt)*xzptsr(j)*za*(smalld)**alphaa*(one-sh(i,j))**(one-alphaa)
             call hunt(xy2pts,nk2pts,toty,jk)
             if (jk .lt. 1) then
                jk = 1
                wgt = one
             elseif (jk .ge. nk2pts) then
                jk = nk2pts-1
                wgt = zero
             else
                wgt = one-(toty-xy2pts(jk))/(xy2pts(jk+1)-xy2pts(jk))
             endif
             disty(jk,nt)   = disty(jk,nt)   +  wgt*f(i,j)
             disty(jk+1,nt) = disty(jk+1,nt) + (one-wgt)*f(i,j)
             income = toty
             rmfood(nt) = rmfood(nt)+xzptsr(j)*za*(smalld)**alphaa* &
                                     (one-sh(i,j))**(one-alphaa)*f(i,j)
          enddo
       enddo
    
 !!! Simulate the model forward 
       ffyr(nt) = pa(nt)*rmfood(nt)/income
       aggfc(:,nt) = aggfc(:,nt) + distc(:,nt)*mur
       aggfy(:,nt) = aggfy(:,nt) + disty(:,nt)*mur
       aggfk(:,nt) = aggfk(:,nt) + distk(:,nt)*mur
    
       do j = 1,nzpts
          jk = 1
          do i = 1,nk2pts
             ca = pa(nt)*sca(i,j) 
             call hunt(xca2pts,nk2pts,ca,jk)
             if (jk .lt. 1) then
                jk = 1
                wgt = one
             elseif (jk .ge. nk2pts) then
                jk = nk2pts-1
                wgt = zero
             else
                wgt = one-(ca-xca2pts(jk))/(xca2pts(jk+1)-xca2pts(jk))
             endif
             aggfca(jk,nt)   = aggfca(jk,nt)   +      wgt *f(i,j)*mur
             aggfca(jk+1,nt) = aggfca(jk+1,nt) + (one-wgt)*f(i,j)*mur
          enddo
       enddo
    enddo !!! end nt loop

!   if (myrank .eq. root) close(1)    
    deallocate(skp,sca,scs,scm,sh,f2,f,jks,wgts,sv)
       
    return
    end subroutine simulation_rural