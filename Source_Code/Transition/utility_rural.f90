!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine v_rural(np,j,x,fj)
      
      use params
      use states
      use arrays
      use prices
      implicit none

      integer, intent(in) :: np,j
      real(dp), dimension(np), intent(in) :: x
      real(dp), intent(out) :: fj
      real(dp) :: ca,cm,cs,utilc,val
      integer :: ierr
      logical :: skip
      real(dp), dimension(1) :: xkp,yval

	  xkp(1) = x(1)
	  cs = x(2)
	  
	  ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
      cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))
      
      if ( ca .gt. abar ) then
      	 utilc = dlog(ca - abar) + psi*dlog(cs) + gamma*dlog(cm)
      else
         utilc = -infty
      endif
      
	  skip = .true.
      call dpchfe(nkpts,xkpts,v(:,nz),vp(:,nz),1,skip,1,xkp,yval,ierr)
      val = utilc+beta*yval(1)
      fj = -val

      return
      end subroutine v_rural
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine dv_rural(np,j,x,fj,dummy)
      
      use params
      use states
      use arrays
      use prices
      implicit none
      
      integer, intent(in) :: np,j
      real(dp), dimension(np), intent(in) :: x
      real(dp), dimension(np), intent(out) :: fj
      real(dp) :: ca,cm,cs
      integer :: ierr
      logical :: skip
      real(dp), dimension(1) :: xkp,yval,yp
      external dummy

	  xkp(1) = x(1)
	  cs = x(2)

      ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
      cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))

	  skip = .true.
      call dpchfd(nkpts,xkpts,v(:,nz),vp(:,nz),1,skip,1,xkp,yval,yp,ierr)
      
	  fj(1) = -beta*yp(1)
!	  fj(2) = -(one/(ca-abar))*(ps/(psi*(one+taua)*pa)) - &
!	           (gamma/cm)*(gamma*ps/(psi*(one+taum)*pm)) - psi/cs
      fj(2) = - (one+gamma+psi)/cs

      return
      end subroutine dv_rural
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine const_rural(np,j,x,gj)
      
      use params
      use states
      use arrays
      use prices

      implicit none
      integer, intent(in) :: np,j
      real(dp), dimension(np), intent(in) :: x
      real(dp), intent(out) :: gj
      real(dp) :: ca,cm,cs
      real(dp), dimension(1) :: xkp

	  xkp(1) = x(1)
	  cs = x(2)
      
      ca = ps(nt)*cs/(psi*(one+taua(nt))*pa(nt)) + abar
      cm = gamma*cs*ps(nt)/(psi*(one+taum(nt))*pm(nt))
      
      gj = (one+taua(nt))*pa(nt)*ca + (one+taum(nt))*pm(nt)*cm + &
                        ps(nt)*cs + pm(nt)*xkp(1) - budget

      return
      end subroutine const_rural
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine dconst_rural(np,j,x,gj,dummy)

      use params
      use states
      use arrays
      use prices
      implicit none

      integer, intent(in) :: np,j
      real(dp), dimension(np), intent(in) :: x
      real(dp), dimension(np), intent(out) :: gj
      real(dp) :: cs
      real(dp), dimension(1) :: xkp

      external dummy

	  xkp(1) = x(1)
	  cs = x(2)

      gj(1) = pm(nt)
	  gj(2) = (one+taua(nt))*pa(nt)*(ps(nt)/(psi*(one+taua(nt))*pa(nt))) + &
	            (one+taum(nt))*pm(nt)*(gamma*ps(nt)/(psi*(one+taum(nt))*pm(nt))) + ps(nt)

      return
      end subroutine dconst_rural