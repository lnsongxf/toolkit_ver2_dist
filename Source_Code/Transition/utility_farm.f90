!===============================================================
	subroutine utility_farm(n,x,f)
	
	use params
    use states
    use arrays
    use prices
    use aggstats
	implicit none
    
    integer,  intent(in) :: n
    real(dp), intent(in), dimension(n) :: x
    real(dp), intent(out), dimension(n) :: f
    
    real(dp) :: cs,csp,ca,cm,el,er,bl,br,xk,xkp
    real(dp) :: xhstp,lam,fk,rst,ra,pist,pia,xha,xhst
    integer  :: ntp
    
    cs = x(1)
    xk = x(2)
    
    ntp = nt + 1
    csp = aggcfs(ntp)
    xkp = aggkf(ntp)
    xhstp = agghst(ntp)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    fk = (one-alphast1-alphast2)*pst*z*(bigd**alphast1)* &
             (xhstp**alphast2)*(xkp**(-alphast1-alphast2))
    
    el = pm(nt)*ps(ntp)*csp/(beta*pm(ntp)*ps(nt)*cs)
    er = one - delta + delta*taust(nt) + (one-taust(nt))*fk/pm(ntp)
    f(1) = el-er
    
    lam = (alphast2*pst*z*(bigd)**alphast1/wf(nt))**(one/(one-alphast2))
    xha = ((one-alphaa1)*pa(nt)*za*(smalld)**(alphaa1)/wf(nt))**(one/alphaa1)
    xhst = lam*xk**((one-alphast1-alphast2)/(one-alphast2))
    rst = pst*(z*(bigd)**alphast1)*(xhst**alphast2)*(xk**(one-alphast1-alphast2))
    pist = rst - wf(nt)*xhst
    
    ra = pa(nt)*(za*(smalld)**alphaa1)*(xha**(one-alphaa1))
    pia = ra - wf(nt)*xha
    
    bl = (one + gamma + psi)*ps(nt)*cs/psi + (one+taua(nt))*pa(nt)*abar + pm(nt)*xkp
    br = (one-taur(nt))*pia + (one-taust(nt))*pist + taust(nt)*delta*pm(nt)*xk + &
                (one-delta)*pm(nt)*xk + tf
    f(2) = bl - br

    return
    end subroutine utility_farm
!===============================================================
!===============================================================
	subroutine utility_farm1(n,x,f)
	
	use params
    use states
    use arrays
    use prices
    use aggstats
	implicit none
    
    integer,  intent(in) :: n
    real(dp), intent(in), dimension(n) :: x
    real(dp), intent(out), dimension(n) :: f
    
    real(dp) :: cs,csp,ca,cm,el,er,bl,br,xk,xkp
    real(dp) :: xhstp,lam,fk,rst,ra,pist,pia,xha,xhst
    integer  :: ntp
    
    cs = x(1)
    xk = x(2)
    
    ntp = nt + 1
    csp = aggcfs(ntp)
    xkp = aggkf(ntp)
    xhstp = agghst(ntp)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    fk = (one-alphast1-alphast2)*pst*z*(bigd**alphast1)* &
             (xhstp**alphast2)*(xkp**(-alphast1-alphast2))
    
    el = pm(nt)*ps(ntp)*csp/(beta*pm(ntp)*ps(nt)*cs)
    ! er = one - delta + delta*taust(nt) + (one-taust(nt))*fk/pm(ntp)
    ! Xin: er should be Euler equation rhs, t+1 
    er = one - delta + delta*taust(ntp) + (one-taust(ntp))*fk/pm(ntp)
    f(1) = el-er
    
    lam = (alphast2*pst*z*(bigd)**alphast1/wf(nt))**(one/(one-alphast2))
    xha = ((one-alphaa1)*pa(nt)*za*(smalld)**(alphaa1)/wf(nt))**(one/alphaa1)
    xhst = lam*xk**((one-alphast1-alphast2)/(one-alphast2))
    rst = pst*(z*(bigd)**alphast1)*(xhst**alphast2)*(xk**(one-alphast1-alphast2))
    pist = rst - wf(nt)*xhst
    
    ra = pa(nt)*(za*(smalld)**alphaa1)*(xha**(one-alphaa1))
    pia = ra - wf(nt)*xha
    
    bl = (one + gamma + psi)*ps(nt)*cs/psi + (one+taua(nt))*pa(nt)*abar + pm(nt)*xkp
    br = (one-taur(nt))*pia + (one-taust(nt))*pist + taust(nt)*delta*pm(nt)*xk + &
                (one-delta)*pm(nt)*xk + tf
    f(2) = bl - br

print'(4f15.6)', f,x
    return
    end subroutine utility_farm1
!===============================================================