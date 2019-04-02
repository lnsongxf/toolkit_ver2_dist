! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of Environmental Variables Used in Computation.
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
    
    module states
      
       use params
       implicit none
       
       logical  :: solveprices
       real(dp) :: xkcur,xzcur,budget
       integer  :: nz

    end module states