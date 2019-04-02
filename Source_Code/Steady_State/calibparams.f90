! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of parameters to be endogeneously calibrated.
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
  
    module calibparams
         
         use params
         implicit none
         
         ! Service Productivity is normalized to one
         real(dp) :: abar       ! Subsistence Level
         real(dp) :: za         ! Agricultural Productivity
         real(dp) :: z          ! Export Sector Productivity
         real(dp) :: zm         ! Manufacturing Productivity

         real(dp) :: sigmar     ! Rural Shock Variance
         real(dp) :: sigmau     ! Urban Shock Variance
         real(dp) :: psi        ! Service Preference
         real(dp) :: gamma      ! Manufacturing Preference

         real(dp) :: taua       ! Agri. Tax
         real(dp) :: taum       ! Manu. Tax = Agri. Tax
         real(dp) :: tauw       ! Labor Income Tax
         real(dp) :: taust      ! Trade Tax
         real(dp) :: taur       ! Corporate Tax = Trade Tax
         real(dp) :: taui       ! Manufacturing Output Tax (not in paper)
      end module calibparams