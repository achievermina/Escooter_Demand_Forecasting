library(mlogit)
library(dplyr)

trip = read.csv("man_only_trip_withCitiaccess.csv")
citi_zone=read.csv('citi_access_modi_only1.csv');

#x= c(-2.52589954564460,8.91003714905797,1.99532538129400,0.716718549744234)
#x=c(-854827182.200000	-529943239.800000	-56961703.6666667	-69685799.6666667)
x= c(-2.52589954564460,8.91003714905797,1.99532538129400,0.716718549744234) #without time 
#x=c(2.49408315692692	-4.26474771155901	-19.3102159583797	-16.4332922740551) #with time


#carppol pt taxi bike walk
constant=c(0, 3.12028, 0.959621, 0.436728, 5.72408)
cost=-0.0616752;
time=c(0.600431, -1.7437, 0, -4.27817, -5.68429)
tsf=-3.14574;
access=-2.66062;
egress=-2.59169;
mu=1/0.012;
constant_auto=-2.20;
cost_auto=cost/mu;                                        
time_auto=0;

constant_smartphone=1.75
age_56=-1.48;
age_7=-2.63;
income=c(-0.478,0.641,0.789)
work=1.08;

V_smartphone=constant_smartphone+trip[7]*age_56+trip[8]*age_7+trip[9]*income[1]+trip[11]*income[2]+trip[12]*income[3]+work*trip[13];
smartphone=exp(V_smartphone)/(exp(V_smartphone)+1);

citi_access=trip$citi_access


#Utility function
V_carpool=constant[1]+cost*trip[29]+time[1]*trip[28]/3600 
V_pt=trip[32]*(constant[2]+cost*trip[33]+time[2]*trip[21]/3600+tsf*trip[34]+access*trip[22]/3600+egress*trip[23]/3600)
V_taxi=constant[3]+cost*trip[16]+time[3]*trip[17]/3600  
V_bike=constant[4]+cost*trip[30]+time[4]*trip[19]/3600  
V_walk=constant[5]+cost*trip[31]+time[5]*trip[18]/3600  
V_citi=citi_access*(x[1]+cost*4.29+time[4]*trip[19]/3600+x[2]*smartphone)
V_escooter=x[3]+cost*(1+0.15*trip[19])+x[4]*smartphone
#V_escooter=x[3]+cost*(1+0.15*trip[19])+x[4]*smartphone+time[4]*trip[19]/3600 

UtilitySum=exp(V_carpool)+exp(V_pt)+exp(V_taxi)+exp(V_bike)+exp(V_walk)+(V_citi)*citi_access+exp(V_escooter)


#mode choice probability
p_carpool=exp(V_carpool)/UtilitySum
p_pt=exp(V_pt)/UtilitySum
p_taxi=exp(V_taxi)/UtilitySum
p_bike=exp(V_bike)/UtilitySum
p_walk=exp(V_walk)/UtilitySum
p_citi=exp(V_citi)/UtilitySum*citi_access
p_escooter=exp(V_escooter)/UtilitySum
                                                     


# of each mode trip by zone
carpool_trips=p_carpool*trip[20]
pt_trips =p_pt*trip[20]
taxi_trips =p_taxi*trip[20]
bike_trips =p_bike*trip[20]
walk_trips =p_walk*trip[20]
citi_trips =p_citi*trip[20]
escooter_trips =p_escooter*trip[20]


Predicted_trip =cbind(carpool_trips,pt_trips,taxi_trips,bike_trips,walk_trips,citi_trips,escooter_trips)
Predicted_trip =Predicted_trip*30
Predicted_trip=cbind(trip$OTAZ,Predicted_trip)
colnames(Predicted_trip)=c("TAZ","carpool_trips","pt_trips","taxi_trips","bike_trips","walk_trips","citi_trips","escooter_trips")

write.csv(Predicted_trip, file ="predicted trip_escooter_monthly_real.csv")
