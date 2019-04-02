!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine simulation_urban
       
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
! ==============================================================================
! Data Dictionary: Policy functions on denser grids
!       skp   : saving policy
!       sca   : food consumption
!       scs   : service consumption
!       scm   : manufacturing consumption
!       sh    : formal labor supply
!       sv    : value function
! ==============================================================================    
    real(dp), allocatable, dimension(:,:) :: skp,sca,scs,scm,sh,sv
! ==============================================================================
! Data Dictionary: total amount of xx on each consumption distribution node
!       fyc   : income
!       fhc   : labor supply to formal sector
!       fkc   : saving
!       fyec  : informal income
!       fycc  : interest income
!       fylc  : formal income
! ==============================================================================        
    real(dp), allocatable, dimension(:) :: fyc,fhc,fkc,fyec,fycc,fylc
    integer :: i,j,k,ite,jk
    real(dp) :: wgt,fmxdif,fsum,ca
    real(dp) :: cmax,cmin,camax,camin,ymax,ymin,cinc,cainc,yinc,totc,toty
    
    allocate(skp(nk2pts,nzpts),sca(nk2pts,nzpts), &
             scs(nk2pts,nzpts),scm(nk2pts,nzpts), &
             sh(nk2pts,nzpts),f1(nk2pts,nzpts), &
             f2(nk2pts,nzpts),jks(nk2pts,nzpts), &
             sv(nk2pts,nzpts), wgts(nk2pts,nzpts))
    allocate(fyc(nk2pts),fhc(nk2pts),fkc(nk2pts), &
             fyec(nk2pts),fycc(nk2pts),fylc(nk2pts))
    fyc = zero
    fhc = zero
    fkc = zero
    fyec = zero
    fylc = zero
    fycc = zero

! --------------------------------------------------------------------------
! Construct Policy Functions on Denser Grids
! --------------------------------------------------------------------------
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

          skp(i,j) = wgt*optku(jk,j) + (one-wgt)*optku(jk+1,j)
          sca(i,j) = wgt*optcau(jk,j) + (one-wgt)*optcau(jk+1,j)
          scs(i,j) = wgt*optcsu(jk,j) + (one-wgt)*optcsu(jk+1,j)
          scm(i,j) = wgt*optcmu(jk,j) + (one-wgt)*optcmu(jk+1,j)
          sh(i,j) = wgt*opthu(jk,j) + (one-wgt)*opthu(jk+1,j)
          sv(i,j) = wgt*yu(jk,j) + (one-wgt)*yu(jk+1,j)
       enddo
    enddo

! --------------------------------------------------------------------------
! Construct Asset-Shock Joint Density Function
! --------------------------------------------------------------------------
! Initialization: for each saving, fraction of households with different
!  shocks equals to the invariant distribution
    f2 = zero
    do i = 1,nk2pts
       f2(i,:) = pinvu(:)/dble(nk2pts)
    enddo

! Calculate position of next period saving
    jk = 1
    do j = 1,nzpts
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

! Compute the stationary distribution on asset and shock
! Iterate of the Density Function
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
                f2(jk,k) = pizu(j,k)*wgt*f1(i,j)+f2(jk,k)
                f2(jk+1,k) = pizu(j,k)*(one-wgt)*f1(i,j)+f2(jk+1,k)
             enddo
          enddo
       enddo
       fmxdif = maxval(dabs(f2-f1))
       fsum = sum(f2)

       if (myrank .eq. root) then
          if (dabs(fsum-one) > error) print*, 'failed urban'
       endif
       ite = ite+1
    enddo
    
    if (myrank .eq. root) then   
       open(1,file='dist_urban.txt',form='formatted')
          do j =1,nzpts
             do i = 1,nk2pts
                write(1,100) f2(i,j),sca(i,j),scs(i,j),scm(i,j),sh(i,j),sv(i,j)
             100   format(6f12.6)
             enddo
          enddo
       close(1)
       open(1,file='dist_urban_labor.txt',form='formatted')
          do j =1,nzpts
             do i = 1,nk2pts
                write(1,101) f2(i,j),sh(i,j)
             101   format(2f12.6)
             enddo
          enddo
       close(1)
!!!!!!!! to get the utility distribution
       open(1,file='dist_urban_utility.txt',form='formatted')
          do j =1,nzpts
             do i = 1,nk2pts
                write(1,102) f2(i,j),sv(i,j)
             102   format(2f12.6)
             enddo
          enddo
       close(1)
!!!!!!!!!!!!!!!!!!! end
    endif

! --------------------------------------------------------------------------
! Construct Density on Consumption/Food consumption, Saving, and Income
! --------------------------------------------------------------------------    
    cmin = pa*abar
    cmax = pa*sca(nk2pts,nzpts)+ps*scs(nk2pts,nzpts)+pm*scm(nk2pts,nzpts)
    cinc = (cmax - cmin)/dble(real(nk2pts-1))
    
    camin = pa*abar
    camax = pa*sca(nk2pts,nzpts)
    cainc = (camax - camin)/dble(real(nk2pts-1))
    
    ymin = pm*re*xk2pts(1) + tu
    ymax = xzptsu(nzpts)*w*sh(nk2pts,nzpts) + pm*re*xk2pts(nk2pts) + tu + &
            xzptsue(nzpts)*ps*zs*(one-sh(nk2pts,nzpts))**(one-alphas)
    yinc = (ymax - ymin)/dble(real(nk2pts-1))
    
    xc2pts(1) = cmin
    xca2pts(1) = camin
    xy2pts(1) = ymin
    do i = 2,nk2pts
       xc2pts(i) = xc2pts(i-1) + cinc
       xca2pts(i) = xca2pts(i-1) + cainc
       xy2pts(i) = xy2pts(i-1) + yinc
    enddo

 !!!compute k,h,ca,cm,cs,dist of c,dist of k
     service = zero
     do j = 1,nzpts
        jk = 1
       do i = 1,nk2pts
          aggcua = aggcua + sca(i,j)*f2(i,j)
          aggcus = aggcus + scs(i,j)*f2(i,j)
          aggcum = aggcum + scm(i,j)*f2(i,j)
          agghu  = agghu  + sh(i,j)*f2(i,j)
          aggku  = aggku  + skp(i,j)*f2(i,j)
          aggwu  = aggwu  + xzptsu(j)*w*sh(i,j)*f2(i,j)
          service = service + xzptsue(j)*zs*(one-sh(i,j))**(one-alphas)*f2(i,j)
          aggeubar = aggeubar + xzptsu(j)*sh(i,j)*f2(i,j)
          eu(1) = eu(1) + sv(i,j)*f2(i,j)

          totc = pa*sca(i,j)+ps*scs(i,j)+pm*scm(i,j)
          
          if (pa*sca(i,j)/totc .gt. 0.466_dp) then
             poverty(1) = poverty(1) + f2(i,j)*muu
             urbanpoverty(1) = urbanpoverty(1) +f2(i,j)
          endif
          if (sca(i,j) .lt. 0.145_dp) then
             poverty(2) = poverty(2) + f2(i,j)*muu
             urbanpoverty(2) = urbanpoverty(2) +f2(i,j)
          endif
          if (pa*sca(i,j) .lt. 2.6_dp) then
             poverty(3) = poverty(3) + f2(i,j)*muu
             urbanpoverty(3) = urbanpoverty(3) +f2(i,j)
          endif
          
! ---------------------------------------------------------------------------------
! Construct distribution on consumption and saving         
! ---------------------------------------------------------------------------------          
          call hunt(xc2pts,nk2pts,totc,jk)
          if (jk < 1) then
             jk = 1
             wgt = one
          elseif (jk >= nk2pts) then
             jk = nk2pts-1
             wgt = zero
          else
             wgt = one-(totc-xc2pts(jk))/(xc2pts(jk+1)-xc2pts(jk))
          endif
          distc(jk)   = distc(jk)   +  wgt*f2(i,j)
          distc(jk+1) = distc(jk+1) + (one-wgt)*f2(i,j)
          
          ! Food Expenditure on every consumption node
          shareca(jk)   = shareca(jk)   +  wgt*pa*sca(i,j)*f2(i,j)*muu
          shareca(jk+1) = shareca(jk+1) + (one-wgt)*pa*sca(i,j)*f2(i,j)*muu

          toty = (one-tauw)*xzptsu(j)*w*sh(i,j) + ps*zs*(one-sh(i,j))**(one-alphas) + &
                pm*re*xk2pts(i) + tu

          fyc(jk)   = fyc(jk)   +  wgt*toty*f2(i,j)
          fyc(jk+1) = fyc(jk+1) + (one-wgt)*toty*f2(i,j)
          
          fhc(jk)   = fhc(jk)   +  wgt*sh(i,j)*f2(i,j)
          fhc(jk+1) = fhc(jk+1) + (one-wgt)*sh(i,j)*f2(i,j)
          
          fkc(jk)   = fkc(jk)   +  wgt*skp(i,j)*f2(i,j)
          fkc(jk+1) = fkc(jk+1) + (one-wgt)*skp(i,j)*f2(i,j)
          
          fyec(jk)   = fyec(jk)   +  wgt*ps*zs*(one-sh(i,j))**(one-alphas)*f2(i,j)
          fyec(jk+1) = fyec(jk+1) + (one-wgt)*ps*zs*(one-sh(i,j))**(one-alphas)*f2(i,j)
          
          fycc(jk)   = fycc(jk)   +  wgt*pm*re*xk2pts(i)*f2(i,j)
          fycc(jk+1) = fycc(jk+1) + (one-wgt)*pm*re*xk2pts(i)*f2(i,j)
          
          fylc(jk)   = fylc(jk)   +  wgt*(one-tauw)*xzptsu(j)*w*sh(i,j)*f2(i,j)
          fylc(jk+1) = fylc(jk+1) + (one-wgt)*(one-tauw)*xzptsu(j)*w*sh(i,j)*f2(i,j)
          
          ! Density on savings
          distk(i) = distk(i) + f2(i,j)
       enddo
     enddo

! ---------------------------------------------------------------------------------
! Construct distribution on income         
! ---------------------------------------------------------------------------------
    do j = 1,nzpts
       jk = 1
       do i = 1,nk2pts

          toty = (one-tauw)*xzptsu(j)*w*sh(i,j) + ps*xzptsue(j)*zs*(one-sh(i,j))**(one-alphas) + &
                pm*re*xk2pts(i) + tu

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

       enddo
    enddo
    
!!!!!!
    if (myrank .eq. root) then
    open(1,file='furban.txt',form='formatted')
    do i = 1,nk2pts
       if ( distc(i) .gt. zero ) then
          write(1,103) xc2pts(i),distc(i),fyc(i)/distc(i),fhc(i)/distc(i), &
                    fkc(i)/distc(i),fyec(i)/distc(i),fylc(i)/distc(i),fycc(i)/distc(i)
       else
          write(1,103) xc2pts(i),distc(i),fyc(i),fhc(i),fkc(i),fyec(i),fylc(i),fycc(i)
       endif
    103   format(10f12.6)
    enddo
    close(1)
    endif

!!! agg dists    
    aggfc = distc*muu
    aggfy = disty*muu
    aggfk = distk*muu
    aggeubar = aggeubar/agghu
    
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
          aggfca(jk)   = aggfca(jk)   +  wgt*f2(i,j)*muu
          aggfca(jk+1) = aggfca(jk+1) + (one-wgt)*f2(i,j)*muu

       enddo
    enddo

    deallocate(fyc,fhc,fkc,fyec,fycc,fylc)
    deallocate(skp,sv,sca,scs,scm,sh,f1,f2,jks,wgts)
    
    return
    end subroutine simulation_urban