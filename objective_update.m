function y=objective_update(x)

trip=csvread('man.csv',1,0);
citi_zone=csvread('citi_access.csv',1,1);
expand=1;    % whether or not expand to 2016

for i=1:size(citi_zone,1)
    citi_zone(i,3)=citi_zone(i,1)*citi_zone(i,2);
end

constant=[0, 3.12028, 0.959621, 0.436728, 5.72408, 0.609463916095560];
cost=-0.0616752;
time=[0.600431, -1.7437, 0, -4.27817, -5.68429, -4.27817];
tsf=-3.14574;
access=-2.66062;
egress=-2.59169;

B_smartphone=-0.552211962397275;

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




citi_access=zeros(size(trip,1),1);
for i=1:size(trip,1)
    if((ismember(trip(i,3),citi_zone(:,3))*ismember(trip(i,4),citi_zone(:,3)))>0)
        citi_access(i)=1;
    end
end



temp=zeros(size(trip,1),7);
logsum1=zeros(size(trip,1),1);
logsum2=zeros(size(trip,1),1);
tour_time=zeros(size(trip,1),1);
tour_cost=zeros(size(trip,1),1);
toursum=zeros(size(trip,1),1);
tour_weight=zeros(size(trip,1),1);

tour_time(1,1)=trip(1,14);
tour_cost(1,1)=trip(1,15);
tour_weight(1,1)=trip(1,20);

for i=1:size(trip,1)
%carpool, pt, taxi, bike, walk, citi, escooter
    temp(i,1)=constant(1)+cost*trip(i,29)+time(1)*trip(i,28)/3600;  %V_carpool
    
    temp(i,2)=trip(i,32)*(constant(2)+cost*trip(i,33)+time(2)*trip(i,21)/3600+tsf*trip(i,34)+access*trip(i,22)/3600+egress*trip(i,23)/3600); %+wait*trip(i,25)/3600)
      %V_pt
    temp(i,3)=constant(3)+cost*trip(i,16)+time(3)*trip(i,17)/3600;  %V_taxi
    
    temp(i,4)=constant(4)+cost*trip(i,30)+time(4)*trip(i,19)/3600;  %V_bike
   
    temp(i,5)=constant(5)+cost*trip(i,31)+time(5)*trip(i,18)/3600;  %V_walk
    
    temp(i,6)=citi_access(i)*(constant(6)+cost*4.29+time(6)*trip(i,19)/3600+B_smartphone*smartphone(i));  %V_citi
    
    temp(i,7)=(x(1)+cost*(1+0.15*time(6))+time(6)*trip(i,19)/3600); %escooter_without_smartphone



    logsum1(i)=log(exp(temp(i,1))+exp(temp(i,2))*trip(i,32)+exp(temp(i,3))+exp(temp(i,4))+exp(temp(i,5))+expand*(exp(temp(i,6))*citi_access(i)))+exp(temp(i,7));
    %logsum with escooter
    
    %logsum2(i)=log(exp(temp(i,1))+exp(temp(i,2))*trip(i,32)+exp(temp(i,3))+exp(temp(i,4))+exp(temp(i,5))+expand*(exp(temp(i,6))*citi_access(i)+exp(temp(i,7))));
    
    if(i>1 && trip(i,36)==0)
        tour_time(i)=tour_time(i-1)+trip(i,14);
        tour_cost(i)=tour_cost(i-1)+trip(i,15);
        tour_weight(i)=tour_weight(i-1)+trip(i,20);
    elseif(i>1 && trip(i,36)>0)
        tour_time(i)=trip(i,14);
        tour_cost(i)=trip(i,15);
        tour_weight(i)=trip(i,20);
    end
    
end



toursum(1,1)=logsum1(1);
for i=2:size(trip,1)
    if(trip(i,36)==0) %first trip(1) or not (0)  
        toursum(i)=toursum(i-1)+logsum1(i);
    else
        toursum(i)=logsum1(i);
    end
end

p_auto=zeros(size(trip,1),1);
p_nonauto=zeros(size(trip,1),1);

%+time_auto*tour_time(i)/3600
for i=1:(size(trip,1)-1)
    if(trip(i+1,36)==1)
        p_auto(i)=exp(constant_auto+cost_auto*tour_cost(i))/(exp(constant_auto+cost_auto*tour_cost(i))+exp(toursum(i)/mu)); %+time_auto*tour_time(i)/3600
        p_nonauto(i)=exp(toursum(i)/mu)/(exp(constant_auto+cost_auto*tour_cost(i))+exp(toursum(i)/mu));
    elseif(i>1 && trip(i)>0)
        p_auto(i)=0;
        p_nonauto(i)=0;
    end
end
%remove +time_auto*tour_time(size(trip,1))/3600
%+time_auto*tour_time(size(trip,1))/3600
p_auto(size(trip,1))=exp(constant_auto+cost_auto*tour_cost(size(trip,1)))/(exp(constant_auto+cost_auto*tour_cost(size(trip,1)))+exp(toursum(size(trip,1))/mu));
p_nonauto(size(trip,1))=exp(toursum(size(trip,1))/mu)/(exp(constant_auto+cost_auto*tour_cost(size(trip,1)))+exp(toursum(size(trip,1))/mu));


for i=(size(trip,1)-1):-1:1
    if(p_auto(i)==0)
        p_auto(i)=p_auto(i+1);
        p_nonauto(i)=p_nonauto(i+1);
    end
end

p_carpool=zeros(size(trip,1),1);
p_pt=zeros(size(trip,1),1);
p_taxi=zeros(size(trip,1),1);
p_bike=zeros(size(trip,1),1);
p_walk=zeros(size(trip,1),1);
p_citi=zeros(size(trip,1),1);
p_escooter=zeros(size(trip,1),1);


for i=1:size(trip,1)
    p_carpool(i)=trip(i,20)*p_nonauto(i)*(exp(temp(i,1))/exp(logsum1(i))); % only logsum1
    
    p_pt(i)=trip(i,20)*p_nonauto(i)*trip(i,32)*(exp(temp(i,2))/exp(logsum1(i)));
    p_taxi(i)=trip(i,20)*p_nonauto(i)*(exp(temp(i,3))/exp(logsum1(i)));
    p_bike(i)=trip(i,20)*p_nonauto(i)*(exp(temp(i,4))/exp(logsum1(i)));
    p_walk(i)=trip(i,20)*p_nonauto(i)*(exp(temp(i,5))/exp(logsum1(i)));
    p_citi(i)=trip(i,20)*expand*p_nonauto(i)*(exp(temp(i,6))/exp(logsum1(i))*citi_access(i));
    p_escooter(i)=trip(i,20)*p_nonauto(i)*(exp(temp(i,7))/exp(logsum1(i)));
end

zone=zeros(1622,16);


escooter_trip= csvread('fake_escooter.csv',1,0); %11,12 column
%citibike_trip = csvread('citi.csv',1,0);

for i=1:size(trip,1)
    zone(trip(i,3),1)=zone(trip(i,3),1)+p_auto(i)*trip(i,20);
    zone(trip(i,3),2)=zone(trip(i,3),2)+p_carpool(i);
    zone(trip(i,3),3)=zone(trip(i,3),3)+p_pt(i);
    zone(trip(i,3),4)=zone(trip(i,3),4)+p_taxi(i);
    zone(trip(i,3),5)=zone(trip(i,3),5)+p_bike(i);
    zone(trip(i,3),6)=zone(trip(i,3),6)+p_walk(i);
    zone(trip(i,3),7)=zone(trip(i,3),7)+p_citi(i);
    zone(trip(i,3),8)=zone(trip(i,3),8)+p_escooter(i);
end



%%% zone to calculate the whole modes
%% if i want only Escooter,no need


for i=1:size(trip,1)
    if(trip(i,27)==1)
        zone(trip(i,3),10)=zone(trip(i,3),10)+trip(i,20);
    elseif(trip(i,27)==2)
        zone(trip(i,3),11)=zone(trip(i,3),11)+trip(i,20);
    elseif(trip(i,27)==3)
        zone(trip(i,3),12)=zone(trip(i,3),12)+trip(i,20);
    elseif(trip(i,27)==4)
        zone(trip(i,3),13)=zone(trip(i,3),13)+trip(i,20);
    elseif(trip(i,27)==5)
        zone(trip(i,3),14)=zone(trip(i,3),14)+trip(i,20);
    elseif(trip(i,27)==6)
        zone(trip(i,3),9)=zone(trip(i,3),9)+trip(i,20);
    end
end




RMSE=zeros(1622,8);
for i=1:1622
    RMSE(i,1)=(zone(i,1)-zone(i,9))^2;
    RMSE(i,2)=(zone(i,2)-zone(i,10))^2;
    RMSE(i,3)=(zone(i,3)-zone(i,11))^2;
    RMSE(i,4)=(zone(i,4)-zone(i,12))^2;
    RMSE(i,5)=(zone(i,5)-zone(i,13))^2;
    RMSE(i,6)=(zone(i,6)-zone(i,14))^2;
    RMSE(i,7)=(zone(i,7)-zone(i,15))^2;
    RMSE(i,8)=(zone(i,8)-zone(i,16))^2;
end
y=sqrt(sum(RMSE(:,7)/1622))+sqrt(sum(RMSE(:,8)/1622));


%minimize sum of (y -guessed_y)^2 for only escooter


