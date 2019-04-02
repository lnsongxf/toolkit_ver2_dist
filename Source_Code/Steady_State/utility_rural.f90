! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Auxillary subroutines used in solving the Rural DP problem.
!   Same structure as the Urban DP. See the comments there.
!
! Data Dictionary:
!   Subroutines:   
!       v_rural     : value function of rural household
!       const_rural : constraint of rural household
!       dv_rural    : derivative of rural value function
!       dconst_rural: derivative of rural constraints
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    03/26/2017                 Legacy Code
!
! ===============================================================================
      subroutine v_rural(np,j,x,fj)
      
      use params
      use calibparams
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
	  
	  ca = ps*cs/(psi*(one+taua)*pa) + abar
      cm = gamma*cs*ps/(psi*(one+taum)*pm)
      
      if ( ca .gt. abar ) then
      	 utilc = dlog(ca - abar) + psi*dlog(cs) + gamma*dlog(cm)
      else
         utilc = -infty
      endif
      
	  skip = .true.
      call dpchfe(nkpts,xkpts,vr(:,nz),vpr(:,nz),1,skip,1,xkp,yval,ierr)
      val = utilc+beta*yval(1)
      fj = -val

      return
      end subroutine v_rural
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine dv_rural(np,j,x,fj,dummy)
      
      use params
      use calibparams
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

      ca = ps*cs/(psi*(one+taua)*pa) + abar
      cm = gamma*cs*ps/(psi*(one+taum)*pm)

	  skip = .true.
      call dpchfd(nkpts,xkpts,vr(:,nz),vpr(:,nz),1,skip,1,xkp,yval,yp,ierr)
      
	  fj(1) = -beta*yp(1)
!	  fj(2) = -(one/(ca-abar))*(ps/(psi*(one+taua)*pa)) - &
!	           (gamma/cm)*(gamma*ps/(psi*(one+taum)*pm)) - psi/cs
      fj(2) = - (one+gamma+psi)/cs

      return
      end subroutine dv_rural
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine const_rural(np,j,x,gj)
      
      use params
      use calibparams
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
      
      ca = ps*cs/(psi*(one+taua)*pa) + abar
      cm = gamma*cs*ps/(psi*(one+taum)*pm)
      
      gj = (one+taua)*pa*ca + (one+taum)*pm*cm + ps*cs + pm*xkp(1) - budget

      return
      end subroutine const_rural
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine dconst_rural(np,j,x,gj,dummy)

      use params
      use calibparams
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

      gj(1) = pm
	  gj(2) = (one+taua)*pa*(ps/(psi*(one+taua)*pa)) + &
	                      (one+taum)*pm*(gamma*ps/(psi*(one+taum)*pm)) + ps

      return
      end subroutine dconst_rural