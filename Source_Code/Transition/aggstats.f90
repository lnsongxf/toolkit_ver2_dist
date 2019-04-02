!*********************************************************************
    module aggstats
       use params
       implicit none

       !real(dp), dimension(ntran) :: dmfood,exfood,rmfood,service,manufacture,output
       !real(dp), dimension(ntran) :: lowca, lowfca,meanca,aggeu
       !real(dp), dimension(ntran) :: personaltax,vattax,businesstax
       !real(dp), dimension(ntran) :: agghu,aggha,agghr,agghst
       !real(dP), dimension(ntran) :: aggku,aggkr,aggkf
       !real(dp), dimension(ntran) :: aggwu,aggwr
       !real(dp), dimension(ntran) :: aggca,aggcm,aggcs
       !
       !real(dp), dimension(ntran) :: aggcfa,aggcfs,aggcfm
       !real(dp), dimension(ntran) :: aggcra,aggcrs,aggcrm
       !real(dp), dimension(ntran) :: aggcua,aggcus,aggcum
       !real(dp), dimension(ntran) :: gdp,agginv,export,aggtax,aggcon
       !real(dp), dimension(ntran) :: aggcu,aggcr,aggcf
       !real(dp), dimension(ntran) :: ffyr
       !
       !real(dp), dimension(ntran) :: raggcfa,raggcfs,raggcfm
       !real(dp), dimension(ntran) :: raggcra,raggcrs,raggcrm
       !real(dp), dimension(ntran) :: raggcua,raggcus,raggcum
       !real(dp), dimension(ntran) :: rgdp,ragginv,rexport,raggtax,raggcon
       !real(dp), dimension(ntran) :: raggcu,raggcr,raggcf
       !
       !real(dp), dimension(ntran) :: ginicu,giniyu,giniku
       !real(dp), dimension(ntran) :: ginicr,giniyr,ginikr
       
       real(dp), allocatable, dimension(:) :: dmfood,exfood,rmfood,service,manufacture,output
       real(dp), allocatable, dimension(:) :: lowca, lowfca,meanca,aggeu
       real(dp), allocatable, dimension(:) :: personaltax,vattax,businesstax
       real(dp), allocatable, dimension(:) :: agghu,aggha,agghr,agghst
       real(dp), allocatable, dimension(:) :: aggku,aggkr,aggkf
       real(dp), allocatable, dimension(:) :: aggwu,aggwr
       real(dp), allocatable, dimension(:) :: aggca,aggcm,aggcs
       
       real(dp), allocatable, dimension(:) :: aggcfa,aggcfs,aggcfm
       real(dp), allocatable, dimension(:) :: aggcra,aggcrs,aggcrm
       real(dp), allocatable, dimension(:) :: aggcua,aggcus,aggcum
       real(dp), allocatable, dimension(:) :: gdp,agginv,export,aggtax,aggcon
       real(dp), allocatable, dimension(:) :: aggcu,aggcr,aggcf
       real(dp), allocatable, dimension(:) :: ffyr
       
       real(dp), allocatable, dimension(:) :: raggcfa,raggcfs,raggcfm
       real(dp), allocatable, dimension(:) :: raggcra,raggcrs,raggcrm
       real(dp), allocatable, dimension(:) :: raggcua,raggcus,raggcum
       real(dp), allocatable, dimension(:) :: rgdp,ragginv,rexport,raggtax,raggcon
       real(dp), allocatable, dimension(:) :: raggcu,raggcr,raggcf
       
       real(dp), allocatable, dimension(:) :: ginicu,giniyu,giniku
       real(dp), allocatable, dimension(:) :: ginicr,giniyr,ginikr       
       real(dp), allocatable, dimension(:) :: ginict,giniyt,ginikt       
    CONTAINS
       subroutine initialized_aggstats

       implicit none
       dmfood = zero
       exfood = zero
       rmfood = zero
       service = zero
       manufacture = zero
       output = zero
       aggeu = zero

       personaltax = zero
       vattax = zero
       businesstax = zero
       
       agghu  = zero
       aggha  = zero
       agghr  = zero
       agghst = zero
       aggku  = zero
       aggkr  = zero
       aggkf  = zero
       aggwu  = zero
       aggwr  = zero
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       aggcus = zero
       aggcrs = zero
       aggcfs = zero
       aggcua = zero
       aggcra = zero
       aggcfa = zero
       aggcum = zero
       aggcrm = zero
       aggcfm = zero

       aggtax = zero
       gdp    = zero
       agginv = zero
       export = zero
       aggcon = zero
       
       aggcu  = zero 
       aggcr  = zero
       aggcf  = zero
       aggca  = zero
       aggcm  = zero
       aggcs  = zero
       
       ffyr   = zero
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       raggcfa = zero
       raggcfs = zero 
       raggcfm = zero
       raggcra = zero
       raggcrs = zero
       raggcrm = zero
       raggcua = zero
       raggcus = zero
       raggcum = zero
       
       raggtax = zero
       rgdp = zero
       ragginv = zero
       rexport = zero
       raggcon = zero

       raggcu = zero
       raggcr = zero
       raggcf = zero
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       ginicu = zero
       giniyu = zero
       giniku = zero
       
       ginicr = zero
       giniyr = zero
       ginikr = zero
       
       ginict = zero
       giniyt = zero
       ginikt = zero

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       end subroutine initialized_aggstats
    end module aggstats