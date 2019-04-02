!==========================================================
!	Maximizing utility by Golden Section
!==========================================================
    double precision function golden(ax,bx,cx,func,tol,xmin)
              
    implicit none
    
    integer, parameter  :: dp = kind(1.0d0)

    real(dp), intent(in)  :: ax,bx,cx,tol
    real(dp), intent(out) :: xmin
    
    real(dp), parameter   :: r=0.61803399_dp
    real(dp), parameter   :: c=1.0_dp - r

    real(dp) :: func
    external :: func

    real(dp) :: x0,x1,x2,x3
    real(dp) :: f1,f2

    x0 = ax
    x3 = cx

    if (dabs(cx-bx) .gt. dabs(bx-ax)) then
       x1 = bx
       x2 = bx+c*(cx-bx)
    else
       x2 = bx
       x1 = bx-c*(bx-ax)
    endif
  
    f1 = func(x1)
    f2 = func(x2)
    
1   if (dabs(x3-x0) .gt. tol*(dabs(x1)+dabs(x2)) .and. dabs(x3-x0) .gt. tol)then

       if (f2 .lt. f1)then
          x0 = x1
          x1 = x2
          x2 = r*x1 + c*x3
          f1 = f2
          f2 = func(x2)
       else
          x3 = x2
          x2 = x1
          x1 = r*x2 + c*x0
          f2 = f1
          f1 = func(x1)
       endif

       goto 1
    endif

    if (f1 .lt. f2)then
       golden = f1
       xmin   = x1
    else
       golden = f2
       xmin   = x2
    endif

    return
    end function golden
