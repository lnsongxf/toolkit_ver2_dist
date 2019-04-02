    module cpiparams
      
       use params
       implicit none    

       real(dp), parameter, dimension(3) :: basketa = (/0.419935_dp,0.182322_dp,0.733051_dp/)
       real(dp), parameter, dimension(3) :: basketm = (/5.907242_dp,2.390269_dp,10.541756_dp/)
       real(dp), parameter, dimension(3) :: baskets = (/0.248535_dp,0.100566_dp,0.443522_dp/)


       real(dp), parameter, dimension(3) :: baseprice = (/18.1211242126_dp,15.4435205611_dp,1.0_dp/)
       real(dp), parameter, dimension(4) :: production = & !domestic food, service, manufacture, export
                            (/0.26537576_dp,0.15228569_dp,4.66774269_dp,1.80255726_dp/)
                  
       real(dp), parameter :: basketcost = 11.3984120685282_dp
    end module cpiparams    
    
   
    
        
        
    
    

    
