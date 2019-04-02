! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Subroutine that solves the dynamic programmin problem of large farms.
!
! Data Dictionary:
!   Output: ca  : food consumption
!           cs  : service consumption
!           cm  : manufacturing consumption
!           xha : domestic food labor demand
!           xhst: exporting food labor demand
!           xk  : capital accumulation
!           ex  : total exporting value 
!           tax : total tax paid by large farm
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
    subroutine farm(ca,cs,cm,xha,xhst,xk,ex,tax)

    use params
    use calibparams
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none

    real(dp), intent(out) :: ca,cs,cm,xha,xhst,xk,ex,tax
    real(dp) :: lam,up,dn,ra,rst,pia,pist,totc,wgt,toty,totk
    integer  :: jk
    
    ! Use the property that k does not change in steady state to solve for
    !   optimal capital/food labor/export labor choice
    lam = (alphast2*pst*z*(bigd)**alphast1/wf)**(one/(one-alphast2))
    up = pst*(one-alphast1-alphast2)*(one-taust)*(lam**alphast2)*z*(bigd)**alphast1
    dn = pm*(one/beta - (one-delta) -delta*taust)
    
    xk = (up/dn)**((one-alphast2)/alphast1)
    xha = ((one-alphaa1)*pa*za*(smalld)**(alphaa1)/wf)**(one/alphaa1)
    xhst = lam*xk**((one-alphast1-alphast2)/(one-alphast2))

! ===============================================================================
! Data Dictionary:
!       ra : food production revenue
!	   pia : food production profit
!      rst : export revenue
!     pist : export profits
! ===============================================================================    
    ra = pa*(za*(smalld)**alphaa1)*(xha**(one-alphaa1))
    pia = ra - wf*xha
    rst = pst*(z*(bigd)**alphast1)*(xhst**alphast2)*(xk**(one-alphast1-alphast2))
    pist = rst - wf*xhst
    up = psi*((one-taur)*pia + (one-taust)*pist + &
            (taust-one)*delta*pm*xk + tf - (one+taua)*pa*abar)
    dn = (one + gamma + psi)*ps
    
    cs = up/dn
    ca = ps*cs/(psi*(one+taua)*pa) + abar
    cm = gamma*ps*cs/(psi*(one+taum)*pm)
    ex = muf*rst
    tax = taur*pia  + taust*pist - taust*delta*pm*xk

!!! ca   
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
    
    aggfca(jk)   = aggfca(jk)   +  wgt*muf
    aggfca(jk+1) = aggfca(jk+1) + (one-wgt)*muf

!!! total consumption    
    jk = 1
    totc = pa*ca + ps*cs + pm*cm
    
    if (pa*ca/totc .gt. 0.466_dp) poverty(1) = poverty(1) + muf
    if (ca .lt. 0.145_dp) poverty(2) = poverty(2) + muf
    if (pa*ca .lt. 2.6_dp) poverty(3) = poverty(3) + muf
    
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
    
    eu(3) = totc
    aggfc(jk)   = aggfc(jk)   +  wgt*muf
    aggfc(jk+1) = aggfc(jk+1) + (one-wgt)*muf
    
    shareca(jk)   = shareca(jk)   +  wgt*pa*ca*muf
    shareca(jk+1) = shareca(jk+1) + (one-wgt)*pa*ca*muf
    
!!! income    
    jk = 1
    toty = (one-taur)*pia+(one-taust)*pist+taust*delta*pm*xk+(one-delta)*pm*xk+tf
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
    
    aggfy(jk)   = aggfy(jk)   +  wgt*muf
    aggfy(jk+1) = aggfy(jk+1) + (one-wgt)*muf
    
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
    
    aggfk(jk)   = aggfk(jk)   +  wgt*muf
    aggfk(jk+1) = aggfk(jk+1) + (one-wgt)*muf
    
    return
    end subroutine farm