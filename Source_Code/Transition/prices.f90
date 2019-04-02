    module prices
      
       use params
       implicit none
       
       !real(dp), dimension(ntran,5)    :: eprice
       !real(dp), dimension(ntran_in,5) :: eprice_in
       !real(dp), dimension(ntran)      :: ps
       !real(dp), dimension(ntran)      :: pa
       !real(dp), dimension(ntran)      :: w
       !real(dp), dimension(ntran)      :: wf
       !real(dp), dimension(ntran)      :: re
       
       real(dp), allocatable, dimension(:,:) :: eprice
       real(dp), allocatable, dimension(:,:) :: eprice_in
       real(dp), allocatable, dimension(:)   :: ps
       real(dp), allocatable, dimension(:)   :: pa
       real(dp), allocatable, dimension(:)   :: w
       real(dp), allocatable, dimension(:)   :: wf
       real(dp), allocatable, dimension(:)   :: re       
    end module prices