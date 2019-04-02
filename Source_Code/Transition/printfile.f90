!===============================================================
	subroutine printoutfile
	
	use params
    use arrays
    use prices
    use aggstats
	implicit none
    
	integer :: nt,j
	
	open(1,file='consumption.txt',form='formatted')
	   do nt = 1,ntran
          write(1,101) aggcua(nt), aggcus(nt), aggcum(nt), &
                       aggcra(nt), aggcrs(nt), aggcrm(nt), &
                       aggcfa(nt), aggcfs(nt), aggcfm(nt)
       		101 format(9f12.6)
       enddo
    close(1)
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
	open(1,file='moments.txt',form='formatted')
	   do nt = 1,ntran
          write(1,102) &
                    ginicu(nt), &
                    ginicr(nt), &
                    ginict(nt), &
                    giniyu(nt), &
                    giniyr(nt), &
                    giniyt(nt), &
                    giniku(nt), &
                    ginikr(nt), &
                    ginikt(nt), &
                    pm(nt)*manufacture(nt)/output(nt), &
                    ps(nt)*muu*service(nt)/output(nt), &
                    export(nt)/gdp(nt), &
                    aggtax(nt)/gdp(nt), &
                    personaltax(nt)/aggtax(nt), &
                    businesstax(nt)/aggtax(nt), &
                    vattax(nt)/aggtax(nt), &
                    lowca(nt)/lowfca(nt)/meanca(nt), &
                    aggcm(nt)/aggcon(nt), &
                    aggcs(nt)/aggcon(nt), &
                    agghu(nt), &
       				aggha(nt), &
       				agghr(nt), &
       				agghst(nt), &
       				aggku(nt), &
       				aggkr(nt), &
                    aggkf(nt), &
                    ps(nt)*aggcus(nt), &
                    ps(nt)*aggcrs(nt), &
                    ps(nt)*aggcfs(nt), &
       				pa(nt)*aggcua(nt), &
       				pa(nt)*aggcra(nt), &
       				pa(nt)*aggcfa(nt), &
       				pm(nt)*aggcum(nt), &
       				pm(nt)*aggcrm(nt), &
                    pm(nt)*aggcfm(nt), &
       				aggcu(nt), &
       				aggcr(nt), &
       				aggcf(nt), &
       				ps(nt)*aggcs(nt), &
       				pa(nt)*aggca(nt), &
       				pm(nt)*aggcm(nt), &
       				gdp(nt), &
       				aggcon(nt), &
       				agginv(nt), &
       				aggtax(nt), &
       				export(nt)
       		102 format(100(f12.6 /))
       enddo
    close(1)

	open(1,file='epout_periods.txt',form='formatted')    
        write(1,'(I3)') ntran
    close(1)    
    
	open(1,file='model_parameters.txt',form='formatted')    
        write(1,104) ntran, &
                     abar, psi, gamma, &
                     sigmar, sigmau, muu, mur, &
                     za, zm, z, &
                     taua(1), tauw(1), taur(1), &
                     taua(ntran), tauw(ntran), taur(ntran)
        104 format(I12 /,100(f12.6 /))
    close(1)
    
    open(2,file='model_param_names.txt',form='formatted')
        write(2,103) 'ntran     ', &
                     'abar      ', &
                     'psi       ', &
                     'gamma     ', &
                     'sigmar    ', &
                     'sigmau    ', &
                     'muu       ', &
                     'mur       ', &
                     'za        ', &
                     'zm        ', &
                     'z         ', &
                     'taua_ss1  ', &
                     'tauw_ss1  ', &
                     'taur_ss1  ', &
                     'taua_ss2  ', &
                     'tauw_ss2  ', &
                     'taur_ss2  '
    close(2)
    
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    open(2,file='variables.txt',form='formatted')
       write(2,103) 'ginicu     ', &
                    'ginicr     ', &
                    'ginict     ', &
                    'giniyu     ', &
                    'giniyr     ', &
                    'giniyt     ', &
                    'giniku     ', &
                    'ginikr     ', &
                    'ginikt     ', &
                    'ind/Output ', &
                    'ser/Output ', &
                    'exp/GDP    ', &
                    'tax/GDP    ', &
                    'ptax/aggtax', &
                    'btax/aggtax', &
                    'vtax/aggtax', &
                    'loca/lofca ', &
                    'cm/tot c   ', &
                    'cs/tot c   ', &
                    'agghu      ', &
       				'aggha      ', &
       				'agghr      ', &
       				'agghst     ', &
       				'aggku      ', &
       				'aggkr      ', &
                    'aggkf      ', &
                    'aggcus     ', &
                    'aggcrs     ', &
                    'aggcfs     ', &
       				'aggcua     ', &
       				'aggcra     ', &
       				'aggcfa     ', &
       				'aggcum     ', &
       				'aggcrm     ', &
       				'aggcfm     ', &
       				'aggcu      ', &
       				'aggcr      ', &
       				'aggcf      ', &
       				'aggcs      ', &
       				'aggca      ', &
       				'aggcm      ', &
       				'gdp        ', &
       				'aggcon     ', &
       				'agginv     ', &
       				'aggtax     ', &
       				'export     '
       		103 format(100(a15 /))     
        close(2)
    return
    
    end subroutine printoutfile