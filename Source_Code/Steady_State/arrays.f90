! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of Auxilliary Variables Used in Computation.
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
    
    module arrays
    
       use params
       implicit none
       
       real(dp), dimension(nval)   :: eprice
       real(dp), dimension(11)     :: weight
       !real(dp), dimension(nkpts)  :: xkpts
       !real(dp), dimension(nk2pts) :: xk2pts
       
       real(dp), allocatable, dimension(:)  :: xkpts
       real(dp), allocatable, dimension(:)  :: xk2pts
       
! ==============================================================================       
! Data Dictionary: xzptsu  : urban formal idiosyncratic shock
!                  xzptsr  : rural own farm idiosyncratic shock
!                  pinvu   : urban shock stationary distribution
!                  pinvr   : rural shock stationary distribution
!                  zpts    : auxillary log-normal shock
!                  xzptsue : urban informal idiosyncratic shock
!                  xhptsu  : labor supply to urban formal
!                  xhptsr  : labor supply to rural formal
!                  pizu    : urban shock transition matrix
!                  pizr    : rural shock transition matrix
! ==============================================================================
       !real(dp), dimension(nzpts)  :: xzptsu,xzptsr,pinvu,pinvr,zpts,xzptsue
       !real(dp), dimension(nzpts)  :: xhptsu,xhptsr
       !real(dp), dimension(nzpts,nzpts) :: pizu,pizr
       real(dp), allocatable, dimension(:)  :: xzptsu,xzptsr,pinvu,pinvr,zpts,xzptsue
       real(dp), allocatable, dimension(:)  :: xhptsu,xhptsr
       real(dp), allocatable, dimension(:,:) :: pizu,pizr       

! ==============================================================================
! Data Dictionary: y2u, yu : varaibles to calculate convergence of value function
!                  optku   : saving policy
!                  opthu   : urban formal labor supply policy
!                  optcsu  : urban service consumption policy
!                  optcmu  : urban manufacturing consumption policy
!                  optcau  : urban agricultural consumption policy
!                  vu      : value function
!                  vpu     : derivative of Piecewise Cubic Hermite Interpolation
!                  optcs2u, optk2u: to calculate convergence of policy functions
! The same structure preserves for rural households.
! ==============================================================================       
       real(dp), allocatable, dimension(:,:) :: y2u,optk2u,vu,vpu,yu,optku,opthu
       real(dp), allocatable, dimension(:,:) :: optcs2u,optcsu,optcmu,optcau
  
       real(dp), allocatable, dimension(:,:) :: y2r,optk2r,vr,vpr,yr,optkr,opthr
       real(dp), allocatable, dimension(:,:) :: optcs2r,optcsr,optcmr,optcar
       
! ==============================================================================
! Data Dictionary: distc/k/y : density of household over consumption/saving/income
!                  xc/y/ca2pts : consumption, income, food consumption policies
!                                on denser grids
!                  aggfc/k/y/ca : density of consumption, saving, income and 
!                                 food consumption in total population (across 
!                                 sectors)
!                  shareca : total expenditure on food at each consumption node
! The same structure preserves for rural households.
! ==============================================================================              
       real(dp), allocatable, dimension(:) :: distc,distk,disty
       real(dp), allocatable, dimension(:) :: xc2pts,xy2pts,xca2pts
       real(dp), allocatable, dimension(:) :: aggfc,aggfk,aggfy,aggfca
       real(dp), allocatable, dimension(:) :: shareca

    end module arrays
