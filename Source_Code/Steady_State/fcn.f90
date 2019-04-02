! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Subroutine that evaluates the model at price x and calculates the 
!       distance between model and data at x.
!
! Data Dictionary:
!   Input:  np: number of market clearing conditions.
!           x:  market prices where the model is evaluated at.
!   Output: fvec: excess demand in each market
!           sumsqfunc: distance between model and data (global variables)
!   File:   Export model results to "moments.txt" and the corresponding
!           variable names to "variables.txt".
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    12/26/2016                 Legacy Code
!
! ===============================================================================
    subroutine fcn(np,x,fvec,iflag)

    use params
    use calibparams
    use cpiparams
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
    
    integer, intent(in) :: np,iflag
    real(dp), intent(in ), dimension(np) :: x
    real(dp), intent(out), dimension(np) :: fvec
    
! ===============================================================================
! Data Dictionary: agghm : aggregate labor in manufacturing
!                  aggkm : aggregate capital in manufacturing
!                  aggtf : total tax paid by large farm
! Auxillary Variables:
!                  huval : urban formal supply
!                  huval2: urban formal supply when informal shock set to 1
!                  hrval : rural formal supply
! ===============================================================================
    real(dp) :: agghm,aggkm,aggtf,huval,hrval,huval2
    integer :: j
    
    include 'mpif.h'
    
    ps = x(1)
    pa = x(2)
    w  = x(3)
    wf = x(4)
    re = x(5)
        
    if (myrank .eq. root) then
       open(1,file='labor.txt',form='formatted')
    endif
    
    do j = 1,nzpts
       huval2 = one - ((ps*zs*(one-alphas))/((one-tauw)*xzptsu(j)*w) )**(one/alphas)
       if (huval2 .lt. zero) huval2 = zero

       huval = one - ((ps*zs*(one-alphas)*xzptsue(j))/((one-tauw)*xzptsu(j)*w) )**(one/alphas)
       if (huval .lt. zero) huval = zero
       
       hrval = one-((pa*xzptsr(j)*za*(dr**alphaa)*(one-alphaa))/((one-tauw)*wf) )**(one/alphaa)
       if (hrval .lt. zero) hrval = zero
       if (myrank .eq. root) then
          write(1,101) hrval,huval
       101   format(3f12.6)
       endif
!print'(4f12.6)', huval,huval2,epsue,xzptsu(j)

       xhptsu(j) = huval
       xhptsr(j) = hrval
    enddo
!Stop


    if (myrank .eq. root) close(1)
    ! All prices must be positive. Throw error message when prices are negative
    if ( x(1) .le. zero .or. x(2) .le. zero .or. x(3) .le. zero .or. &
             x(4) .le. zero .or. x(5) .le. zero ) then
       fvec = infty
    else
    
       call initialized_aggstats
       
! ===============================================================================
! Solving Urban Problems       
! ===============================================================================              
       allocate (y2u(nkpts,nzpts),optk2u(nkpts,nzpts),vu(nkpts,nzpts),&
                 vpu(nkpts,nzpts),yu(nkpts,nzpts),optku(nkpts,nzpts),&
                 opthu(nkpts,nzpts),optcs2u(nkpts,nzpts),optcsu(nkpts,nzpts), &
                 optcau(nkpts,nzpts),optcmu(nkpts,nzpts))
       
       if (myrank .eq. root) then 
          WRITE (*,*) 'Start Urban Problem'
       endif
       call urban
       
       allocate(aggfc(nk2pts),aggfca(nk2pts),aggfk(nk2pts),aggfy(nk2pts),shareca(nk2pts))
       aggfc = zero
       aggfca = zero
       aggfk = zero
       aggfy = zero
       shareca = zero
       
       allocate(distc(nk2pts),disty(nk2pts),distk(nk2pts),&
                xc2pts(nk2pts),xca2pts(nk2pts),xy2pts(nk2pts))
       distc = zero
       disty = zero
       distk = zero
       
       call simulation_urban
       call getgini(distc,xc2pts,nk2pts,ginicu)
       call getgini(disty,xy2pts,nk2pts,giniyu)
       call getgini(distk,xk2pts,nk2pts,giniku)
       
       deallocate(distc,disty,distk)
       deallocate(y2u,optk2u,vu,vpu,yu,optku,opthu,optcs2u,optcsu,optcau,optcmu)
    
! ===============================================================================
! Solving Rural Problems       
! ===============================================================================           
       allocate (y2r(nkpts,nzpts),optk2r(nkpts,nzpts),vr(nkpts,nzpts),&
                 vpr(nkpts,nzpts),yr(nkpts,nzpts),optkr(nkpts,nzpts),&
                 opthr(nkpts,nzpts),optcs2r(nkpts,nzpts),optcsr(nkpts,nzpts), &
                 optcar(nkpts,nzpts),optcmr(nkpts,nzpts))
       
       if (myrank .eq. root) then 
          WRITE (*,*) 'Start Rural Problem'
       endif
       call rural
       
       allocate(distc(nk2pts),disty(nk2pts),distk(nk2pts))
       distc = zero
       disty = zero
       distk = zero

       call simulation_rural
       call getgini(distc,xc2pts,nk2pts,ginicr)
       call getgini(disty,xy2pts,nk2pts,giniyr)
       call getgini(distk,xk2pts,nk2pts,ginikr)
       
       deallocate(distc,disty,distk)
       deallocate(y2r,optk2r,vr,vpr,yr,optkr,opthr,optcs2r,optcsr,optcar,optcmr)

! ===============================================================================
! Solving Farm Problems       
! ===============================================================================              
       call farm(aggcfa,aggcfs,aggcfm,aggha,agghst,aggkf,exfood,aggtf)

! ===============================================================================
! Calculate Aggregate Statistics and Excess Demand
! ===============================================================================              
       export = exfood
       dmfood = mur*rmfood + muf*za*(smalld**alphaa1)*(aggha**(one-alphaa1))
       aggkm = muu*aggku + mur*aggkr
       agghm = muu*agghu
       manufacture = zm*(aggkm**alpham)*(agghm**(one-alpham))
   
       output = pa*dmfood + exfood + ps*muu*service + pm*manufacture

!print'(4f16.8)',dmfood, exfood,muu*service,manufacture

       lowca = zero
       lowfca = zero
       meanca = zero
       j = 1
       
       do while ( lowfca .lt. 0.25_dp )
          lowca = lowca + shareca(j)
          lowfca = lowfca + aggfc(j)
          meanca = meanca + aggfc(j)*xc2pts(j)
          j = j + 1
       enddo

       call getgini(aggfc,xc2pts,nk2pts,aggginic)
       call getgini(aggfy,xy2pts,nk2pts,aggginiy)
       call getgini(aggfk,xk2pts,nk2pts,aggginik)
       
       deallocate(xc2pts,xy2pts,xca2pts,aggfc,aggfca,aggfk,aggfy,shareca)
       
!       if (myrank .eq. root) then
!          print'(2(a12,f12.6))','aggkm',aggkm,'agghm',agghm
!          print*,  pa*dmfood+exfood, ps*muu*service, pm*manufacture
!       endif

! --------------------------------------------------------------------------------------------
! Excess Demand of:
!   1. service
!   2. domestic food
!   3. urban labor market
!   4. rural labor market
!   5. capital market
! --------------------------------------------------------------------------------------------
       fvec(1) = muu*aggcus+mur*aggcrs+muf*aggcfs - muu*service
       fvec(2) = muu*aggcua+mur*aggcra+muf*aggcfa - dmfood
       fvec(3) = (one-taui)*pm*zm*(one-alpham)*(aggkm**alpham)*(agghm**(-alpham)) - w*aggeubar
       !fvec(4) = mur*agghr - muf*(aggha+agghst)
       fvec(4) = muf*(aggha+agghst) - mur*agghr
       fvec(5) = (one-taui)*pm*zm*alpham*(aggkm**(alpham-one))*(agghm**(one-alpham)) - (delta + re)
    endif

    if (myrank .eq. root) then
        WRITE (*,*) ''
        WRITE (*,*) 'Market Clearing Conditions: '
        WRITE (*,174) ' ','Service.Y', 'Agri.Y', 'Manu.W', 'Farm.W', 'Interest'
        WRITE (*,173) 'Error = ',fvec(:)
        WRITE (*,173) 'Price = ',x
            173     FORMAT (a12,5f10.4)     
            174     FORMAT (a12,5a10)            
       !print'(a12,5f12.6)', 'fvec = ',fvec(:)
       !print'(a12,5f12.6)', 'x    = ',x
    endif
    
    aggca = muu*aggcua + mur*aggcra + muf*aggcfa
    aggcm = muu*aggcum + mur*aggcrm + muf*aggcfm
    aggcs = muu*aggcus + mur*aggcrs + muf*aggcfs
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    aggcu = pa*aggcua + pm*aggcum + ps*aggcus
    aggcr = pa*aggcra + pm*aggcrm + ps*aggcrs
    aggcf = pa*aggcfa + pm*aggcfm + ps*aggcfs
       
    aggcon = muu*aggcu + mur*aggcr + muf*aggcf
    agginv = delta*pm*(muu*aggku+mur*aggkr+muf*aggkf) + pubk
       
    personaltax = tauw*(muu*aggwu + mur*aggwr)
    vattax = taua*pa*(muu*aggcua + mur*aggcra + muf*aggcfa) + &
                taum*pm*(muu*aggcum + mur*aggcrm + muf*aggcfm)
    businesstax = muf*aggtf + taui*pm*manufacture
    aggtax = vattax + personaltax + businesstax - (muu*tu + mur*tr + muf*tf) &
             - pubk
    gdp = aggcon + agginv + aggtax
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   Calculate CPI
    cpi = (one+taua)*pa*(muu*basketa(1)+mur*basketa(2)+muf*basketa(3)) + &
                     ps*(muu*baskets(1)+mur*baskets(2)+muf*baskets(3)) + &
          (one+taum)*pm*(muu*basketm(1)+mur*basketm(2)+muf*basketm(3))
!if (myrank .eq. root) print*, cpi,basketcost
    cpi = cpi*100/basketcost

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    weight(1 ) = 0.3_dp
    weight(2 ) = 5.0_dp
    weight(3 ) = 8.0_dp
    weight(4 ) = 5.0_dp
    weight(5 ) = 3.0_dp
    weight(6 ) = 2.2_dp
    weight(7 ) = 3.3_dp
    weight(8 ) = 6.0_dp
    weight(9 ) = 1.0_dp
    weight(10) = 1.0_dp
    weight(11) = 5.0_dp

!    sumsqfunc(1 ) = (lowca/meanca - 0.52_dp)
!    sumsqfunc(2 ) = (ps*aggcs/aggcon - 0.21_dp)
!    sumsqfunc(3 ) = (pm*aggcm/aggcon - 0.33_dp)
!    sumsqfunc(4 ) = (ginicr - 0.26_dp)
!    sumsqfunc(5 ) = (ginicu - 0.4_dp)
!    sumsqfunc(6 ) = (vattax/aggtax - 0.45_dp)
!    sumsqfunc(7 ) = (businesstax/aggtax - 0.30_dp)
!    sumsqfunc(8 ) = (personaltax/aggtax - 0.17_dp)
!    sumsqfunc(9 ) = (ps*muu*service/output - 0.16_dp)
!    sumsqfunc(10) = (pm*manufacture/output - 0.33_dp)
!    sumsqfunc(11) = (export/gdp - 0.083_dp)
!!    sumsqfunc(6 ) = (aggtax/gdp - 0.08_dp)     ! Modified by Xin Tang

    calimodel(1) = lowca/meanca
    calimodel(2) = ps*aggcs/aggcon
    calimodel(3) = pm*aggcm/aggcon
    calimodel(4) = ginicr
    calimodel(5) = ginicu
    calimodel(6) = aggtax/gdp
    calimodel(7) = businesstax/aggtax
    calimodel(8) = personaltax/aggtax
    calimodel(9) = ps*muu*service/output
    calimodel(10) = pm*manufacture/output
    calimodel(11) = export/gdp

    sumsqfunc(1 ) = (calimodel(1) - calidata(1))
    sumsqfunc(2 ) = (calimodel(2) - calidata(2))
    sumsqfunc(3 ) = (calimodel(3) - calidata(3))
    sumsqfunc(4 ) = (calimodel(4) - calidata(4))
    sumsqfunc(5 ) = (calimodel(5) - calidata(5))
    sumsqfunc(6 ) = (calimodel(6) - calidata(6))
    !sumsqfunc(6 ) = (vattax/aggtax - 0.45_dp)
    sumsqfunc(7 ) = (calimodel(7) - calidata(7))
    sumsqfunc(8 ) = (calimodel(8) - calidata(8))
    sumsqfunc(9 ) = (calimodel(9) - calidata(9))
    sumsqfunc(10) = (calimodel(10) - calidata(10))
    sumsqfunc(11) = (calimodel(11) - calidata(11))
    
    !sumsqfunc(1 ) = (lowca/meanca - cali_lowca_c)
    !sumsqfunc(2 ) = (ps*aggcs/aggcon - cali_cs_c)
    !sumsqfunc(3 ) = (pm*aggcm/aggcon - cali_cm_c)
    !sumsqfunc(4 ) = (ginicr - cali_gini_r)
    !sumsqfunc(5 ) = (ginicu - cali_gini_u)
    !!sumsqfunc(6 ) = (vattax/aggtax - 0.45_dp)
    !sumsqfunc(7 ) = (businesstax/aggtax - cali_corp_tax)
    !sumsqfunc(8 ) = (personaltax/aggtax - cali_inc_tax)
    !sumsqfunc(9 ) = (ps*muu*service/output - cali_cs_y)
    !sumsqfunc(10) = (pm*manufacture/output - cali_cm_y)
    !sumsqfunc(11) = (export/gdp - cali_ex_y)
    !sumsqfunc(6 ) = (aggtax/gdp - cali_tax_gdp)     ! Tam used 6' tax/gdp, paper uses 6 vat/tax.
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (myrank .eq. root) call printfiles
    
    return
    end subroutine fcn 