[xxz fvalz] = fminunc(@fh_aggr_z, [log(za), log(zm), log(z)],...
    OP);
% [xxz fval1] = fminunc(@fh_aggr_z, [log(za), log(zm), log(z)]);
za = exp(xxz(1)) ;
zm = exp(xxz(2)) ;
z  = exp(xxz(3)) ;

disp('Calibrated Values: za, zm, z');
disp([za, zm, z]);
zstring = ['Sum of Square of Difference: ',num2str(fvalz, '%6.4f')];
disp(zstring);