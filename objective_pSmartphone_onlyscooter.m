function y=objective_pSmartphone_onlyscooter(x)
%x=[0];

%%짜잘하게 고친게 많음 


trip=csvread('man_only_trip.csv',1,0);
citi_zone=csvread('citi_2018_access.csv',1,1);
expand=1;    % whether or not expand to 2016


constant=[0, 3.12028, 0.959621, 0.436728, 5.72408];
cost=-0.0616752;
time=[0.600431, -1.7437, 0, -4.27817, -5.68429];
tsf=-3.14574;
access=-2.66062;
egress=-2.59169;

mu=1/0.012;
constant_auto=-2.20;
cost_auto=cost/mu;
time_auto=0;



constant_smartphone=1.75;
age_56=-1.48;
age_7=-2.63;
income=[-0.478,0.641,0.789];
work=1.08;
smartphone=zeros(size(trip,1),0);

for i=1:size(trip,1)
    V_smartphone=constant_smartphone+trip(i,7)*age_56+trip(i,8)*age_7+trip(i,9)*income(1)+trip(i,11)*income(2)+trip(i,12)*income(3)+work*trip(i,13);
    smartphone(i)=exp(V_smartphone)/(exp(V_smartphone)+1);
end




for i=1:size(citi_zone,1)
    citi_zone(i,3)=citi_zone(i,1)*citi_zone(i,2);
end


citi_access=zeros(size(trip,1),1);
for i=1:size(trip,1)
    if(citi_zone(trip(i,3),2)*citi_zone(trip(i,4),2)>0)
        citi_access(i)=1;
    end
end



temp=zeros(size(trip,1),7);
UtilitySum1=zeros(size(trip,1),1);
UtilitySum2=zeros(size(trip,1),1);


for i=1:size(trip,1)
%carpool, pt, taxi, bike, walk, citi, escooter
    temp(i,1)=constant(1)+cost*trip(i,29)+time(1)*trip(i,28)/3600;  %V_carpool
    
    temp(i,2)=trip(i,32)*(constant(2)+cost*trip(i,33)+time(2)*trip(i,21)/3600+tsf*trip(i,34)+access*trip(i,22)/3600+egress*trip(i,23)/3600); %+wait*trip(i,25)/3600)
      %V_pt
    temp(i,3)=constant(3)+cost*trip(i,16)+time(3)*trip(i,17)/3600;  %V_taxi
    
    temp(i,4)=constant(4)+cost*trip(i,30)+time(4)*trip(i,19)/3600;  %V_bike
   
    temp(i,5)=constant(5)+cost*trip(i,31)+time(5)*trip(i,18)/3600;  %V_walk
    
    %temp(i,6)=citi_access(i)*(x(1)+cost*4.29+time(4)*trip(i,19)/3600+x(2)*smartphone(i));  %V_citi
    
    temp(i,7)=x(1)+cost*(1+0.15*trip(i,19)/60); %escooter

    %Utility without escooter -no smartphone
    UtilitySum1(i)=exp(temp(i,1))+exp(temp(i,2))+exp(temp(i,3))+exp(temp(i,4))+exp(temp(i,5));%+citi_access(i)*exp(temp(i,6));
    
    %Utility with escooter - smartphone

    UtilitySum2(i)=exp(temp(i,1))+exp(temp(i,2))+exp(temp(i,3))+exp(temp(i,4))+exp(temp(i,5))+exp(temp(i,7));%+citi_access(i)*exp(temp(i,6)));
    
end

p_carpool=zeros(size(trip,1),1);
p_pt=zeros(size(trip,1),1);
p_taxi=zeros(size(trip,1),1);
p_bike=zeros(size(trip,1),1);
p_walk=zeros(size(trip,1),1);
%p_citi=zeros(size(trip,1),1);
p_escooter=zeros(size(trip,1),1);
p_check=zeros(size(trip,1),1);


%mode choice probability
for i=1:size(trip,1)
    p_carpool(i)=(exp(temp(i,1))/UtilitySum2(i)*smartphone(i)+(1-smartphone(i))*exp(temp(i,1))/UtilitySum1(i));
    p_pt(i)=(exp(temp(i,2))/UtilitySum2(i)*smartphone(i)+(1-smartphone(i))*exp(temp(i,2))/UtilitySum1(i));
    p_taxi(i)=(exp(temp(i,3))/UtilitySum2(i)*smartphone(i)+(1-smartphone(i))*exp(temp(i,3))/UtilitySum1(i));
    p_bike(i)=(exp(temp(i,4))/UtilitySum2(i)*smartphone(i)+(1-smartphone(i))*exp(temp(i,4))/UtilitySum1(i));
    p_walk(i)=(exp(temp(i,5))/UtilitySum2(i)*smartphone(i)+(1-smartphone(i))*exp(temp(i,5))/UtilitySum1(i));
    %p_citi(i)=(exp(temp(i,6))/UtilitySum2(i)*citi_access(i))*smartphone(i)+(1-smartphone(i))*exp(temp(i,6))/UtilitySum1(i)*citi_access(i);
    p_escooter(i)=exp(temp(i,7))/UtilitySum2(i)*smartphone(i);
    p_check(i) = p_carpool(i)+p_pt(i)+p_taxi(i)+p_bike(i)+ p_walk(i)+p_escooter(i);%p_citi(i)+

end


escooter_trip= csvread('TAZ_mod2_daily_98.csv',1,1);
citibike_trip = csvread('citi_Man_only.csv',1,1);

zone=zeros(size(trip,1),16);

% predicted number of trips
for i=1:size(trip,1)
    zone(trip(i,3),2)=zone(trip(i,3),2)+p_carpool(i)*trip(i,20);
    zone(trip(i,3),3)=zone(trip(i,3),3)+p_pt(i)*trip(i,20);
    zone(trip(i,3),4)=zone(trip(i,3),4)+p_taxi(i)*trip(i,20);
    zone(trip(i,3),5)=zone(trip(i,3),5)+p_bike(i)*trip(i,20);
    zone(trip(i,3),6)=zone(trip(i,3),6)+p_walk(i)*trip(i,20);
    %zone(trip(i,3),7)=zone(trip(i,3),7)+p_citi(i)*trip(i,20);
    zone(trip(i,3),8)=zone(trip(i,3),8)+p_escooter(i)*trip(i,20);
end




% zone to calculate the whole modes
% if i want only Escooter,no need


% number of real trip 
for i=1:size(trip,1)
    if(trip(i,27)==1)  
        zone(trip(i,3),10)=zone(trip(i,3),10)+trip(i,20); %carpool
    elseif(trip(i,27)==2)
        zone(trip(i,3),11)=zone(trip(i,3),11)+trip(i,20); %pt
    elseif(trip(i,27)==3)
        zone(trip(i,3),12)=zone(trip(i,3),12)+trip(i,20); %taxi
    elseif(trip(i,27)==4)
        zone(trip(i,3),13)=zone(trip(i,3),13)+trip(i,20); %bike
    elseif(trip(i,27)==5)
        zone(trip(i,3),14)=zone(trip(i,3),14)+trip(i,20); %walk
    end
end


for i=1:size(citibike_trip,1)
    zone(citibike_trip(i,1),15)= zone(citibike_trip(i,1),15)+citibike_trip(i,3);
end

for i=1:size(escooter_trip,1)
    zone(escooter_trip(i,1),16)= zone(escooter_trip(i,1),16)+escooter_trip(i,3);
end

for i=1:318
    RMSE(i,2)=(zone(i,2)-zone(i,10))^2;
    RMSE(i,3)=(zone(i,3)-zone(i,11))^2;
    RMSE(i,4)=(zone(i,4)-zone(i,12))^2;
    RMSE(i,5)=(zone(i,5)-zone(i,13))^2;
    RMSE(i,6)=(zone(i,6)-zone(i,14))^2;
    %RMSE(i,7)=(zone(i,7)-zone(i,15))^2; %citi
    RMSE(i,8)=(zone(i,8)-zone(i,16))^2; %escooter
end
y=sum(RMSE(:,2))+sum(RMSE(:,3))+sum(RMSE(:,4))+sum(RMSE(:,5))+sum(RMSE(:,6))+sum(RMSE(:,8));%+sum(RMSE(i,7));




