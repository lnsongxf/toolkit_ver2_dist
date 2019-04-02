!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Tauchen
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    subroutine tauchen(n,amean,rho,sigmaeps,xm,Z,Zprob)
            
    implicit none

    integer,  parameter :: dp = kind(1.0d0)
    integer,  intent(in) :: n
    real(dp), intent(in) :: amean
    real(dp), intent(in) :: rho
    real(dp), intent(in) :: sigmaeps
    real(dp), intent(in) :: xm
    real(dp), intent(out), dimension(n) :: Z
    real(dp), intent(out), dimension(n,n) :: Zprob

    real(dp), parameter :: one=1.0_dp, two = 2.0_dp   
    real(8) :: zstep,mean
    integer :: i,j,k

    real(dp)  :: cdf_normal
    external :: cdf_normal

    Z(n) = xm*dsqrt(sigmaeps**two/(one - rho**two))
    Z(1)  = -Z(n)
    zstep = (Z(n) - Z(1))/dble(real(n - 1))

    do i = 2,(n-1)
       Z(i) = Z(1) + zstep*dble(real(i - 1))
    enddo

    mean = amean/(one-rho)
    do i = 1,n
       Z(i) = Z(i) + mean
    enddo

    do j = 1,n
        do k = 1,n
           if (k .eq. 1) then
              Zprob(j,k) = cdf_normal((Z(1)-amean-rho*Z(j) + zstep/two)/sigmaeps)
           elseif (k .eq. n) then
              Zprob(j,k) = one - cdf_normal((Z(n)-amean -rho*Z(j)-zstep/two)/sigmaeps)
           else
              Zprob(j,k) = cdf_normal((Z(k)-amean-rho * Z(j) + zstep/two)/sigmaeps) - &
                               cdf_normal((Z(k)-amean-rho * Z(j) - zstep/two)/sigmaeps)
           endif
        enddo
    enddo

	return
    end subroutine tauchen
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   normal distribution pdf
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    double precision function cdf_normal(x)
    
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    real(dp), intent(in) :: x 
    
    cdf_normal =0.5_dp*derfc(-x/dsqrt(2.0_dp))
    
    return
    end function cdf_normal
 !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~