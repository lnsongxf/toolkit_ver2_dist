!*********************************************************************
    module mpiparams
       implicit none
       integer, parameter :: root = 0
       integer :: myrank,nprocs,mpiierr
       integer :: jsta, jend
       integer :: ipack,iget
    end module mpiparams