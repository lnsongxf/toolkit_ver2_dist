[xxt fvalt] = fminunc(@fh_aggr_t, [taua, tauw, taur], OP);
% [xxt fvalt] = fminunc(@fh_aggr_t, [taua, tauw, taur]);
taua = xxt(1);
tauw = xxt(2);
taur = xxt(3);

disp('Calibrated Values: taua, tauw, taur');
disp([taua, tauw, taur]);
tstring = ['Sum of Square of Difference: ',num2str(fvalt, '%6.4f')];
disp(tstring);