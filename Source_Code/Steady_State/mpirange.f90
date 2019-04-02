!===============================================================
!  PARA_RANGE
!===============================================================
    subroutine mpirange(nend,nprocs,irank,ista,iend)

    implicit none
    
    integer, intent(in)  :: nend
    integer, intent(in)  :: nprocs
    integer, intent(in)  :: irank
    integer, intent(out) :: ista
    integer, intent(out) :: iend
 
    integer :: n0
    
   !Determines the number of grid point assigned to each node
    do n0=1,nend
       if (n0*nprocs >= nend) exit
    end do
   !Compute Nstart and Nend
    ista = irank*n0+1
    iend = MIN((irank+1)*n0,nend) 
      
    return
    end subroutine mpirange