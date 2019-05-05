x0 = [0];
A=[];
b=[];

[x1,fval1] = fmincon(@objective_update,x0,A,b);

