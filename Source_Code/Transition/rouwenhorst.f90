      subroutine rouwenhorst(rho,pi,n)
      implicit none
      real*8, intent(in) :: rho
      integer, intent(in) :: n
      real*8, dimension(n,n), intent(out) :: pi
      real*8, allocatable :: pmat1(:,:),pmat2(:,:),pmat3(:,:),t1(:,:),t2(:,:),t3(:,:),t4(:,:)
      real*8 :: p,q
      integer :: i,j
      
      p = (1.0D+00+rho)/2.0D+00
      q = p
      
      allocate (pmat1(2,2))
      pmat1(1,1) = p
      pmat1(1,2) = 1.0D+00-p
      pmat1(2,1) = 1.0D+00-q
      pmat1(2,2) = q
      
      do i = 3,n
         allocate(pmat2(i,i),pmat3(i,i),t1(i,i),t2(i,i),t3(i,i),t4(i,i))
         t1 = 0.0D+00
         t2 = 0.0D+00
         t3 = 0.0D+00
         t4 = 0.0D+00
         t1(1:i-1,1:i-1) = pmat1
         t2(1:i-1,2:i) = pmat1
         t3(2:i,1:i-1) = pmat1
         t4(2:i,2:i) = pmat1
         pmat2 = p*t1+(1.0D+00-p)*t2+(1.0D+00-q)*t3+q*t4
         pmat3(1,:) = pmat2(1,:)
         do j = 2,i-1
            pmat3(j,:) = pmat2(j,:)/2.0D+00
         end do
         pmat3(i,:) = pmat2(i,:)
         deallocate (pmat1)
         allocate(pmat1(i,i))
         pmat1 = pmat3
         deallocate (pmat2,pmat3,t1,t2,t3,t4)
      end do
      
      pi = pmat1
      
      return
      end