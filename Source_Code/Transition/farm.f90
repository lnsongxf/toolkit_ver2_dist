    subroutine farm

    use params
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    use zbrent_opt
    implicit none

    real(dp) :: lam,ra,rst,pia,pist,totc,wgt,toty,totk,tax,up,dn
    real(dp) :: xkfp,ex,ca,cs,cm,xha,xhst,xk
    integer  :: jk,ntp

!   Common block
    COMMON/NS11BD/LP,LPD,IERR
    INTEGER LP,LPD,IERR

!   Subroutine parameters
    integer, parameter :: n=2
    integer :: maxfun,iprint
    integer, dimension(n) :: iw
    real(dp) :: step,stpmax,acc
    real(dp), dimension(n) :: x, f
    real(dp), dimension(n,n) :: a
    real(dp), dimension(n*(2*n+5)) :: wm
    
    external :: utility_farm,utility_farm1
    
    do nt = ntran,1,-1
       
       if ( nt .eq. 1 .or. nt .eq. ntran) then
          lam = (alphast2*pst*z*(bigd)**alphast1/wf(nt))**(one/(one-alphast2))
          up = pst*(one-alphast1-alphast2)*(one-taust(nt))*(lam**alphast2)*z*(bigd)**alphast1
          dn = pm(nt)*(one/beta - (one-delta) -delta*taust(nt))
    
          xk = (up/dn)**((one-alphast2)/alphast1)
          xha = ((one-alphaa1)*pa(nt)*za*(smalld)**(alphaa1)/wf(nt))**(one/alphaa1)
          xhst = lam*xk**((one-alphast1-alphast2)/(one-alphast2))
    
          ra = pa(nt)*(za*(smalld)**alphaa1)*(xha**(one-alphaa1))
          pia = ra - wf(nt)*xha
          rst = pst*(z*(bigd)**alphast1)*(xhst**alphast2)*(xk**(one-alphast1-alphast2))
          pist = rst - wf(nt)*xhst
          up = psi*((one-taur(nt))*pia + (one-taust(nt))*pist + &
                  (taust(nt)-one)*delta*pm(nt)*xk + tf - (one+taua(nt))*pa(nt)*abar)
          dn = (one + gamma + psi)*ps(nt)
    
          cs = up/dn
          ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
          cm = gamma*ps(nt)*cs/(psi*(one+taum(nt))*pm(nt))
          ex = muf*rst
          tax = taur(nt)*pia  + taust(nt)*pist - taust(nt)*delta*pm(nt)*xk
!print'(i5,5f14.8)', nt,xk,cs,xhst
       else
          ntp = nt + 1
    !     Set parameters
          STEP   = 1.0d-6
          STPMAX = 10.0_dp
          ACC    = 1.0d-8
          MAXFUN = 10000
          IPRINT = 0
          
          cs = aggcfs(ntp)
          xk = aggkf(ntp)
          x(1) = cs
          x(2) = xk

    !     Call NS11 to perform solution
          call NS11AD(utility_farm,n,x,f,a,step,stpmax,acc,maxfun,iprint,wm,iw)

          IF(IERR.NE.0) THEN
             print*, IERR,nt
             !call utility_farm1(n,x,f)
             cs = aggcfs(ntp)
             xk = aggkf(ntp)
             x(1) = cs
             x(2) = xk
             call NS11AD(utility_farm1,n,x,f,a,step,stpmax,acc,maxfun,iprint,wm,iw)
             STOP
          END IF

          cs = x(1)
          ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
          cm = gamma*ps(nt)*cs/(psi*(one+taum(nt))*pm(nt))

          xha = ((one-alphaa1)*pa(nt)*za*(smalld)**(alphaa1)/wf(nt))**(one/alphaa1)
          ra = pa(nt)*(za*(smalld)**alphaa1)*(xha**(one-alphaa1))
          pia = ra - wf(nt)*xha

          xk = x(2)
          lam = (alphast2*pst*z*(bigd)**alphast1/wf(nt))**(one/(one-alphast2))
          xhst = lam*xk**((one-alphast1-alphast2)/(one-alphast2))
!print'(i5,7f12.6)', nt,lam,xk,lam*xk,(alphast2*pst*z*(bigd)**alphast1/wf(nt)),wf(nt)
          rst = pst*(z*(bigd)**alphast1)*(xhst**alphast2)*(xk**(one-alphast1-alphast2))
          pist = rst - wf(nt)*xhst
          ex = muf*rst

          tax = taur(nt)*pia  + taust(nt)*pist - taust(nt)*delta*pm(nt)*xk
       endif
!!!!! ca   
       jk = 1
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
    
       aggfca(jk,nt)   = aggfca(jk,nt)   +  wgt*muf
       aggfca(jk+1,nt) = aggfca(jk+1,nt) + (one-wgt)*muf
!!! total consumption    
       jk = 1
       totc = pa(nt)*ca + ps(nt)*cs + pm(nt)*cm
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
    
       aggfc(jk,nt)   = aggfc(jk,nt)   +  wgt*muf
       aggfc(jk+1,nt) = aggfc(jk+1,nt) + (one-wgt)*muf
    
!!! income    
       jk = 1
       toty = (one-taur(nt))*pia+(one-taust(nt))*pist + &
               taust(nt)*delta*pm(nt)*xk+(one-delta)*pm(nt)*xk + tf
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
    
       aggfy(jk,nt)   = aggfy(jk,nt)   +  wgt*muf
       aggfy(jk+1,nt) = aggfy(jk+1,nt) + (one-wgt)*muf
    
!!! capital
       jk = 1
       totk = xk
       call hunt(xk2pts,nk2pts,totk,jk)
       if (jk .lt. 1) then
          jk = 1
          wgt = one
       elseif (jk .ge. nk2pts) then
          jk = nk2pts-1
          wgt = zero
       else
          wgt = one-(totk-xk2pts(jk))/(xk2pts(jk+1)-xk2pts(jk))
       endif
    
       aggfk(jk,nt)   = aggfk(jk,nt)   +  wgt*muf
       aggfk(jk+1,nt) = aggfk(jk+1,nt) + (one-wgt)*muf
    
    
       aggcfa(nt) = ca
       aggcfs(nt) = cs
       aggcfm(nt) = cm
       aggha(nt) = xha
       agghst(nt) = xhst
       aggkf(nt) = xk
       exfood(nt) = ex
       businesstax(nt) = tax
       
!       print'(i5,5f16.10)', nt,xk,cs,xhst
    enddo
    
    return
    end subroutine farm