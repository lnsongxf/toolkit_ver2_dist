! ----------------------------------------------------------------------------
!                           PROGRAM DESCRIPTION
! ----------------------------------------------------------------------------
! Purpose:
!    - Module of finding the root of a 1-D Function 
! using Van Wijngaarden¨CDekker¨CBrent Method.
!    - Adapted from Numerical Recipe Eds 2.
!
! Author:
!     Xuan S. Tam @ City Univeristy of Hong Kong
!     Commented by: Xin Tang @ IMF, Spring 2017
!
! Record of Revisions:
!       Date                Description of Changes
!   ===========        =================================
!    12/26/2016                 Legacy Code
!
! ===============================================================================   
    
    module zbrent_opt
    implicit none
     CONTAINS 
      double precision function zbrent(func,x1,x2,tol)
          
      implicit none
          
      real(8), intent(in) :: x1
      real(8), intent(in) :: x2
      real(8), intent(in) :: tol
          
      integer, parameter :: itmax = 100
      real(8), parameter :: EPS = 3.0d-8
      integer :: iter
      real(8) :: fa,fb,fc
      real(8) :: a, b, c
      real(8) :: d,e,p,q,r,s,tol1,xm
     
      interface
         double precision function func(x)
         implicit none
         real(8), intent(in) :: x
         end function func
      end interface
          
      a  = x1
      b  = x2
      fa = func(a)
      fb = func(b)
          
      if ( (fa .gt. 0.0d0 .and. fb .gt. 0.0d0) .or. &
           (fa .lt. 0.0d0 .and. fb .lt. 0.0d0))then
         write(*,*) 'root must be bracketed for zbrent'
         stop
      endif
          
      c  = b
      fc = fb
          
      do iter=1,ITMAX
         if ((fb .gt. 0.0d0 .and. fc .gt. 0.0d0) .or. &
             (fb .lt. 0.0d0 .and. fc .lt. 0.0d0))then
            c  = a
            fc = fa
            d  = b-a
            e  = d
         endif
         if (dabs(fc) .lt. dabs(fb)) then
            a  = b
            b  = c
            c  = a
            fa = fb
            fb = fc
            fc = fa
         endif
            
         tol1 = 2.0d0*EPS*dabs(b)+0.5d0*tol
         xm   = 0.5d0*(c-b)
            
         if (dabs(xm) .le. tol1 .or. fb .eq. 0.0d0)then
            zbrent = b
            return
         endif
             
         if (dabs(e) .ge. tol1 .and. dabs(fa) .gt. dabs(fb)) then
            s = fb/fa
            if (a .eq. c) then
               p = 2.0d0*xm*s
               q = 1.0d0-s
            else
               q = fa/fc
               r = fb/fc
               p = s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
               q = (q-1.0d0)*(r-1.0d0)*(s-1.0d0)
            endif
                
            if (p .gt. 0.0d0) q = -q
            p = dabs(p)
            if (2.0d0*p .lt. min(3.0d0*xm*q-dabs(tol1*q),dabs(e*q))) then
               e = d
               d = p/q
            else
               d = xm
               e = d
            endif
         else
            d = xm
            e = d
         endif
             
         a  = b
         fa = fb
         if (dabs(d) .gt. tol1) then
            b = b+d
         else
            b = b+sign(tol1,xm)
         endif
         fb = func(b)
      enddo
      print*, 'zbrent exceeding maximum iterations'
      zbrent = b
      return
      end function zbrent
     !end contains
    end module zbrent_opt