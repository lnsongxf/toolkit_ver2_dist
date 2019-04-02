! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!   Auxillary subroutines used in solving the Urban DP problem.
!
! Data Dictionary:
!   Subroutines:   
!       v_urban     : value function of urban household
!       const_urban : constraint of urban household
!       dv_urban    : derivative of urban value function
!       dconst_urban: derivative of urban constraints
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
      subroutine v_urban(np,j,x,fj)
! ===============================================================================
! Data Dictionary:
!       np : dimension of choice variables
!        j : dimension of objective functions
!        x : vector of choice variables
!       fj : value of objective function (could be vector)
! ===============================================================================

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
      ! Subroutine "dpchife" is the double-precision routine for
      !  evaluating a Piecewise Cubic Hermite Function given the 
      !  values and derivatives.
      call dpchfe(nkpts,xkpts,vu(:,nz),vpu(:,nz),1,skip,1,xkp,yval,ierr)
      val = utilc+beta*yval(1)
      fj = -val

      return
      end subroutine v_urban
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine dv_urban(np,j,x,fj,dummy)

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
      call dpchfd(nkpts,xkpts,vu(:,nz),vpu(:,nz),1,skip,1,xkp,yval,yp,ierr)
      
	  fj(1) = -beta*yp(1)   ! yp(1) is the derivative of value function wrt k'
!	  fj(2) = -(one/(ca-abar))*(ps/(psi*(one+taua)*pa)) - &
!	           (gamma/cm)*(gamma*ps/(psi*(one+taum)*pm)) - psi/cs
      ! Here the log-linear property is used.
      fj(2) = - (one+gamma+psi)/cs
      return
      end subroutine dv_urban
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine const_urban(np,j,x,gj)

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
      end subroutine const_urban
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine dconst_urban(np,j,x,gj,dummy)
      ! Dimension 1 is saving, 2 is consumption
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
      end subroutine dconst_urban