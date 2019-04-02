!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine simulation_rural
    
    use params
    use calibparams
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
       
    integer, parameter :: maxit = 100000
    real(dp), allocatable, dimension(:,:) :: f1,f2,jks,wgts
    real(dp), allocatable, dimension(:,:) :: skp,sca,scs,scm,sh,sv
    real(dp), allocatable, dimension(:) :: fyc,fhc,fkc,fyec,fycc,fylc
    integer :: i,j,k,ite,jk
    real(dp) :: wgt,fmxdif,fsum,income,ca
    real(dp) :: totc,toty
    
    allocate(skp(nk2pts,nzpts),sca(nk2pts,nzpts), &
             scs(nk2pts,nzpts),scm(nk2pts,nzpts), &
             sh(nk2pts,nzpts),f1(nk2pts,nzpts), &
             f2(nk2pts,nzpts),jks(nk2pts,nzpts), &
             sv(nk2pts,nzpts),wgts(nk2pts,nzpts))

    allocate(fyc(nk2pts),fhc(nk2pts),fkc(nk2pts), &
             fyec(nk2pts),fycc(nk2pts),fylc(nk2pts))
    fyc = zero
    fhc = zero
    fkc = zero
    fyec = zero
    fylc = zero
    fycc = zero

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

          skp(i,j) = wgt*optkr(jk,j) + (one-wgt)*optkr(jk+1,j)
          sca(i,j) = wgt*optcar(jk,j) + (one-wgt)*optcar(jk+1,j)
          scs(i,j) = wgt*optcsr(jk,j) + (one-wgt)*optcsr(jk+1,j)
          scm(i,j) = wgt*optcmr(jk,j) + (one-wgt)*optcmr(jk+1,j)
          sh(i,j) = wgt*opthr(jk,j) + (one-wgt)*opthr(jk+1,j)
          sv(i,j) = wgt*yr(jk,j) + (one-wgt)*yr(jk+1,j)
       enddo
    enddo
        
    f2 = zero
    do i = 1,nk2pts
       f2(i,:) = pinvr(:)/dble(real(nk2pts))
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
    
    ite = 1
    fmxdif = infty   
    do while ( ite .le. maxit .and. fmxdif .gt. error) 
       f1 = f2
       f2 = zero
       do j = 1,nzpts
          do i = 1,nk2pts
             jk = jks(i,j)
             wgt = wgts(i,j)
             do k = 1,nzpts
                f2(jk,k) = pizr(j,k)*wgt*f1(i,j)+f2(jk,k)
                f2(jk+1,k) = pizr(j,k)*(one-wgt)*f1(i,j)+f2(jk+1,k)
             enddo
          enddo
       enddo
       fmxdif = maxval(dabs(f2-f1))
       fsum = sum(f2)
       
       if ( myrank .eq. root) then
          if (dabs(fsum-one) > error) print*, 'failed rural'
       endif
       ite = ite+1
    enddo
    
    if (myrank .eq. root) then   
       open(1,file='dist_rural.txt',form='formatted')
          do j =1,nzpts
             do i = 1,nk2pts
                write(1,100) f2(i,j),sca(i,j),scs(i,j),scm(i,j),sh(i,j),sv(i,j)
             100   format(6f12.6)
             enddo
          enddo
       close(1)
        open(1,file='dist_rural_labor.txt',form='formatted')
         do j =1,nzpts
          do i = 1,nk2pts
            write(1,101) f2(i,j),sh(i,j)
             101   format(2f12.6)
          enddo
        enddo
       close(1)
!!!!!!!! to get the utility distribution
       open(1,file='dist_rural_utility.txt',form='formatted')
          do j =1,nzpts
             do i = 1,nk2pts
                write(1,103) f2(i,j),sv(i,j)
             103   format(2f12.6)
             enddo
          enddo
       close(1)
!!!!!!!!!!!!!!!!!!! end
    endif

 !!!compute k,h,ca,cm,cs,distributions
    income = zero
    do j = 1,nzpts
       jk = 1
       do i = 1,nk2pts
          aggcra = aggcra + sca(i,j)*f2(i,j)
          aggcrs = aggcrs + scs(i,j)*f2(i,j)
          aggcrm = aggcrm + scm(i,j)*f2(i,j)
          agghr  = agghr  + sh(i,j)*f2(i,j)
          aggkr  = aggkr  + skp(i,j)*f2(i,j)
          aggwr  = aggwr  + wf*sh(i,j)*f2(i,j)
          eu(2) = eu(2) + sv(i,j)*f2(i,j)
          
          totc = pa*sca(i,j)+ps*scs(i,j)+pm*scm(i,j)
          
          if (pa*sca(i,j)/totc .gt. 0.466_dp) then
             poverty(1) = poverty(1) + f2(i,j)*mur
             ruralpoverty(1) = ruralpoverty(1) + f2(i,j)
          endif
          if (sca(i,j) .lt. 0.145_dp) then
             poverty(2) = poverty(2) + f2(i,j)*mur
             ruralpoverty(2) = ruralpoverty(2) + f2(i,j)
          endif
          if (pa*sca(i,j) .lt. 2.6_dp) then
             poverty(3) = poverty(3) + f2(i,j)*mur
             ruralpoverty(3) = ruralpoverty(3) + f2(i,j)
          endif
          
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
          distc(jk)   = distc(jk)   +  wgt*f2(i,j)
          distc(jk+1) = distc(jk+1) + (one-wgt)*f2(i,j)
          
          shareca(jk)   = shareca(jk)   +  wgt*pa*sca(i,j)*f2(i,j)*mur
          shareca(jk+1) = shareca(jk+1) + (one-wgt)*pa*sca(i,j)*f2(i,j)*mur
          
          toty = (one-tauw)*wf*sh(i,j) + pm*re*xk2pts(i) + tr + &
                  pa*xzptsr(j)*za*(smalld)**alphaa*(one-sh(i,j))**(one-alphaa)

          fyc(jk)   = fyc(jk)   +  wgt*toty*f2(i,j)
          fyc(jk+1) = fyc(jk+1) + (one-wgt)*toty*f2(i,j)
          
          fhc(jk)   = fhc(jk)   +  wgt*sh(i,j)*f2(i,j)
          fhc(jk+1) = fhc(jk+1) + (one-wgt)*sh(i,j)*f2(i,j)
          
          fkc(jk)   = fkc(jk)   +  wgt*skp(i,j)*f2(i,j)
          fkc(jk+1) = fkc(jk+1) + (one-wgt)*skp(i,j)*f2(i,j)
          
          fyec(jk)   = fyec(jk)   +  wgt*pa*xzptsr(j)*za*(smalld)**alphaa*(one-sh(i,j))**(one-alphaa)*f2(i,j)
          fyec(jk+1) = fyec(jk+1) + (one-wgt)*pa*xzptsr(j)*za*(smalld)**alphaa*(one-sh(i,j))**(one-alphaa)*f2(i,j)
          
          fycc(jk)   = fycc(jk)   +  wgt*pm*re*xk2pts(i)*f2(i,j)
          fycc(jk+1) = fycc(jk+1) + (one-wgt)*pm*re*xk2pts(i)*f2(i,j)
          
          fylc(jk)   = fylc(jk)   +  wgt*(one-tauw)*wf*sh(i,j)*f2(i,j)
          fylc(jk+1) = fylc(jk+1) + (one-wgt)*(one-tauw)*wf*sh(i,j)*f2(i,j)
          
          distk(i) = distk(i) + f2(i,j)
       enddo
    enddo
!!!!!!!    
    do j = 1,nzpts
       jk = 1
       do i = 1,nk2pts

          toty = (one-tauw)*wf*sh(i,j) + pm*re*xk2pts(i) + tr + &
                  pa*xzptsr(j)*za*(smalld)**alphaa*(one-sh(i,j))**(one-alphaa)
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
          disty(jk)   = disty(jk)   +  wgt*f2(i,j)
          disty(jk+1) = disty(jk+1) + (one-wgt)*f2(i,j)
          
          income = toty
          rmfood = rmfood+xzptsr(j)*za*(smalld)**alphaa*(one-sh(i,j))**(one-alphaa)*f2(i,j)
       enddo
    enddo
    
    if (myrank .eq. root) then
       open(1,file='frural.txt',form='formatted')
       do i = 1,nk2pts
       if ( distc(i) .gt. zero ) then
          write(1,102) xc2pts(i),distc(i),fyc(i)/distc(i),fhc(i)/distc(i), &
                    fkc(i)/distc(i),fyec(i)/distc(i),fylc(i)/distc(i),fycc(i)/distc(i)
       else
          write(1,102) xc2pts(i),distc(i),fyc(i),fhc(i),fkc(i),fyec(i),fylc(i),fycc(i)
       endif
    102   format(10f12.6)
       enddo
       close(1)
    endif
!!!!
    ffyr = pa*rmfood/income
    aggfc = aggfc + distc*mur
    aggfy = aggfy + disty*mur
    aggfk = aggfk + distk*mur
!if (myrank .eq. root) then
!    write (*,*) 'ffyr = ', ffyr
!endif    
    do j = 1,nzpts
       jk = 1
       do i = 1,nk2pts

          ca = pa*sca(i,j) 
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
          aggfca(jk)   = aggfca(jk)   +  wgt*f2(i,j)*mur
          aggfca(jk+1) = aggfca(jk+1) + (one-wgt)*f2(i,j)*mur

       enddo
    enddo
    
    deallocate(fyc,fhc,fkc,fyec,fycc,fylc)
    deallocate(skp,sca,scs,scm,sh,f1,f2,jks,sv,wgts)
       
    return
    end subroutine simulation_rural