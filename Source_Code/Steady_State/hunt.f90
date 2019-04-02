   subroutine hunt(xx,n,x,jlo)
    ! Search with Correlated Values
    ! See Numerical Recipe 2nd eds, section 3.4.
   implicit none
   integer, intent(inout) :: jlo
   integer, intent(in) :: n
   real*8, intent(in) :: x
   real*8, dimension(n), intent(in) :: xx
   integer :: inc,jhi,jm
   logical :: ascnd

   ascnd = (xx(n) >= xx(1))
   if (jlo <=0 .or. jlo > n) then
      jlo = 0
      jhi = n+1
   else
      inc = 1
      if (x >= xx(jlo) .eqv. ascnd) then
         do
            jhi = jlo+inc
            if (jhi > n) then
               jhi = n+1
               exit
            else
               if (x < xx(jhi) .eqv. ascnd) exit
               jlo = jhi
               inc = inc+inc
            endif
         end do
      else
         jhi = jlo
         do
            jlo = jhi-inc
            if (jlo < 1) then
               jlo = 0
               exit
            else
               if (x >= xx(jlo) .eqv. ascnd) exit
               jhi = jlo
               inc = inc+inc
            endif
         end do
      endif
   endif
   
   do
      if (jhi-jlo <= 1) then
         if (x == xx(n)) jlo = n-1
         if (x == xx(1)) jlo = 1
         exit
      else
         jm = (jhi+jlo)/2
         if (x >= xx(jm) .eqv. ascnd) then
            jlo = jm
         else
            jhi = jm
         endif
      endif
   end do
   
   end subroutine hunt