!**********************************************************************!
!                                                                      !
! June 5, 2015                                                         !
!                                                                      !
!**********************************************************************!  
    program multisectors
    
    use params
    use states
    use arrays
    use prices
    use aggstats
    use mpiparams
    implicit none
    
    integer, parameter :: n = 11
    integer, parameter :: m = n
	real(dp), dimension(m) :: f
	real(dp), dimension(n) :: x
	real(dp) :: xkinc,btime,etime
	integer  :: i,j
    
    ! Read parameters from files, used for toolkit version
    real(dp), dimension(12) :: par
    real(dp), dimension(5)  :: policy_ss1
    real(dp), dimension(5)  :: policy_ss2
    integer, dimension(2)   :: length_in
    
    integer :: endo_flag, extend_flag         
            
    include 'mpif.h'

    call MPI_INIT(mpiierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,mpiierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,mpiierr)
    
    call cpu_time(btime)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    xkpts(1) = 0.00015_dp
    xkinc = zero
    do j = 1,60
       xkinc = xkinc+0.0065_dp*dble(real(j))
       do i = (j-1)*8+2,j*8+1
          xkpts(i) = xkpts(i-1)+xkinc
       enddo
    enddo
    
    xk2pts(1) = xkpts(1)
    xkinc = (xkpts(nkpts)-xkpts(1))/dble(real(nk2pts-1))
    do i = 2,nk2pts
       xk2pts(i) = xk2pts(i-1)+xkinc
    enddo

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    call mpirange(nzpts,nprocs,myrank,jsta,jend)
    ipack = nkpts*(jend-jsta+1)
    iget  = nkpts*nzpts
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    ! Read-in Data
    open(1,file='endo_flag.txt',form='formatted')
	    read(1,111) endo_flag
	111  format(I1)
	close(1)
    
    open(2,file='extend_flag.txt',form='formatted')
	    read(2,111) extend_flag
	close(2)    
    
    open(3,file='param_matlab.txt',form='formatted')
	    read(3,'(F16.10)') par
	close(3)
    
    open(4,file='fiscal1_matlab.txt',form='formatted')
	read(4,'(F16.10)') policy_ss1
	close(4)
    
    open(5,file='fiscal2_matlab.txt',form='formatted')
	read(5,'(F16.10)') policy_ss2
	close(5)
    
 !   open(5,file='price1_matlab.txt',form='formatted')
	!read(5,'(F16.10)') prices_ss1
	!close(5)
 !   
 !   open(6,file='price2_matlab.txt',form='formatted')
	!read(6,'(F16.10)') prices_ss2
	!close(6)    
    
    open(6,file='length_matlab.txt',form='formatted')
	read(6,'(I2)') length_in
	close(6)        
    
    ! constant parameters
    psi    = par(1)
    gamma  = par(2)
    sigmar = par(3)
    sigmau = par(4)
    za     = par(5)
    zm     = par(6)
    z      = par(7)
    muu    = par(8)
    mur    = par(9)
    muf    = par(10)
    abar   = par(11)
    xtol_minpack = par(12)
    
    ! length of transition
    ntran = length_in(1)
    ntran_in = length_in(2)
    
    ! logic variables
    if (endo_flag == 0) then
        equilibrium = .true.
    else
        equilibrium = .false.
    endif
    
    if (extend_flag == 0) then
        extend = .false.
    else
        extend = .true.
    endif
    
    ! Allocate arrays
    allocate(taua(ntran),taum(ntran),tauw(ntran),&
        taust(ntran),taur(ntran),taui(ntran))    
    allocate(tu(ntran),tr(ntran))
    
    allocate(dmfood(ntran),exfood(ntran),rmfood(ntran),&
        service(ntran),manufacture(ntran),output(ntran))
    allocate(lowca(ntran),lowfca(ntran),&
        meanca(ntran),aggeu(ntran))
    allocate(personaltax(ntran),vattax(ntran),businesstax(ntran))
    allocate(agghu(ntran),aggha(ntran),&
        agghr(ntran),agghst(ntran))
    allocate(aggku(ntran),aggkr(ntran),aggkf(ntran))
    allocate(aggwu(ntran),aggwr(ntran))
    allocate(aggca(ntran),aggcm(ntran),aggcs(ntran))
    
    allocate(aggcfa(ntran),aggcfs(ntran),aggcfm(ntran))
    allocate(aggcra(ntran),aggcrs(ntran),aggcrm(ntran))
    allocate(aggcua(ntran),aggcus(ntran),aggcum(ntran))
    allocate(gdp(ntran),agginv(ntran),export(ntran),aggtax(ntran),aggcon(ntran))
    allocate(aggcu(ntran),aggcr(ntran),aggcf(ntran))
    allocate(ffyr(ntran))
    
    allocate(raggcfa(ntran),raggcfs(ntran),raggcfm(ntran))
    allocate(raggcra(ntran),raggcrs(ntran),raggcrm(ntran))
    allocate(raggcua(ntran),raggcus(ntran),raggcum(ntran))
    allocate(rgdp(ntran),ragginv(ntran),rexport(ntran),&
        raggtax(ntran),raggcon(ntran))
    allocate(raggcu(ntran),raggcr(ntran),raggcf(ntran))
    
    allocate(ginicu(ntran),giniyu(ntran),giniku(ntran))
    allocate(ginicr(ntran),giniyr(ntran),ginikr(ntran))
    allocate(ginict(ntran),giniyt(ntran),ginikt(ntran))
    
    allocate(pm(ntran))
    pm = one
    
    allocate(eprice(ntran,5),eprice_in(ntran_in,5))
    allocate(ps(ntran),pa(ntran),w(ntran),wf(ntran),re(ntran))
    
    ! continue to pass parameter values
    ! new steady state
    taua = policy_ss2(1)
    taur = policy_ss2(2)
    tauw = policy_ss2(3)
    tu   = policy_ss2(4)
    tr   = policy_ss2(5)
    ! old steady state
    taua(1) = policy_ss1(1)
    taur(1) = policy_ss1(2)
    tauw(1) = policy_ss1(3)
    tu(1)   = policy_ss1(4)
    tr(1)   = policy_ss1(5)

    taum = taua
    taust = taur
    taui = taur*0.365_dp
    
    ! auxilliary parameters
    nval = 5*(ntran-2)
    ldfjac = nval
    lr = nval*(nval+1)/2    
    
    if ( myrank .eq. root) then
        write (*,'(A21,I3)') ' Transition Period = ', ntran
    endif
    
    call calfun(m,n,f,x)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if (myrank .eq. root) then   
       open(1,file='eprices_out.txt',form='formatted')
          do i = 1,ntran
             write(1,100) (eprice(i,j), j=1,5)
          enddo
       100   format(5f16.10)
       close(1)
    endif  

    call cpu_time(etime)

    if (myrank .eq. root) then 
       print*, 'Time ',(etime-btime)/60.0_dp
       print*, 'finished'
    endif
    
    call MPI_FINALIZE(mpiierr)  
    end program multisectors
    