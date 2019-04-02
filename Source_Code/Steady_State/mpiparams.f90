! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Module of Environmental Variables for Parallel Computing.
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

    
    !*********************************************************************
    module mpiparams
       implicit none
       integer, parameter :: root = 0
       integer :: myrank,nprocs,mpiierr
       integer :: jsta, jend
       integer :: ipack,iget
    end module mpiparams