########revenue management -1-2. escooter_pSmartphone only scooter_mod2



trip = read.csv("man_only_trip_withCitiaccess.csv")

#x=c(-1.48857760272012)  #3600
x=c(-1.23869722597643) #60


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

trip$V_smartphone=constant_smartphone+trip[7]*age_56+trip[8]*age_7+trip[9]*income[1]+trip[11]*income[2]+trip[12]*income[3]+work*trip[13];
trip$smartphone=exp(trip$V_smartphone)/(exp(trip$V_smartphone)+1);

#Utility function
trip$V_carpool=constant[1]+cost*trip[29]+time[1]*trip[28]/3600 
trip$V_pt=trip[32]*(constant[2]+cost*trip[33]+time[2]*trip[21]/3600+tsf*trip[34]+access*trip[22]/3600+egress*trip[23]/3600)
trip$V_taxi=constant[3]+cost*trip[16]+time[3]*trip[17]/3600  
trip$V_bike=constant[4]+cost*trip[30]+time[4]*trip[19]/3600  
trip$V_walk=constant[5]+cost*trip[31]+time[5]*trip[18]/3600  
#trip$V_citi=trip$citi_access*(x[1]+cost*4.29+time[4]*trip[19]/3600+x[2]*trip$smartphone)
#60 or 3600
trip$V_escooter=x[1]+cost*(1+0.15*trip[19]/60)

#without Smartphone, without Escooter
trip$UtilitySum1=exp(trip$V_carpool)+exp(trip$V_pt)+exp(trip$V_taxi)+exp(trip$V_bike)+exp(trip$V_walk)
#with Smartphone, with Escooter
trip$UtilitySum2=exp(trip$V_carpool)+exp(trip$V_pt)+exp(trip$V_taxi)+exp(trip$V_bike)+exp(trip$V_walk)+exp(trip$V_escooter)




#mode choice probability
trip$p_carpool=exp(trip$V_carpool)/trip$UtilitySum2*trip$smartphone+(1-trip$smartphone)*exp(trip$V_carpool)/trip$UtilitySum1
trip$p_pt=exp(trip$V_pt)/trip$UtilitySum2*trip$smartphone+(1-trip$smartphone)*exp(trip$V_pt)/trip$UtilitySum1
trip$p_taxi=exp(trip$V_taxi)/trip$UtilitySum2*trip$smartphone+(1-trip$smartphone)*exp(trip$V_taxi)/trip$UtilitySum1
trip$p_bike=exp(trip$V_bike)/trip$UtilitySum2*trip$smartphone+(1-trip$smartphone)*exp(trip$V_bike)/trip$UtilitySum1
trip$p_walk=exp(trip$V_walk)/trip$UtilitySum2*trip$smartphone+(1-trip$smartphone)*exp(trip$V_walk)/trip$UtilitySum1
#trip$p_citi=exp(trip$V_citi)/trip$UtilitySum*citi_access
trip$p_escooter=exp(trip$V_escooter)/trip$UtilitySum2*trip$smartphone
trip$check=trip$p_carpool+trip$p_pt+trip$p_taxi+trip$p_bike+trip$p_walk+trip$p_escooter#+p_citi




# of each mode trip by zone
trip$carpool_trips=trip$p_carpool*trip[20]
trip$pt_trips =trip$p_pt*trip[20]
trip$taxi_trips =trip$p_taxi*trip[20]
trip$bike_trips =trip$p_bike*trip[20]
trip$walk_trips =trip$p_walk*trip[20]
#trip$citi_trips =trip$p_citi*trip[20]
trip$escooter_trips =trip$p_escooter*trip[20]


#Check the total trips

Predicted_trip =cbind(trip$carpool_trips,trip$pt_trips,trip$taxi_trips,trip$bike_trips,trip$walk_trips,trip$escooter_trips)
Predicted_trip =Predicted_trip*30
Predicted_trip=cbind(trip$OTAZ,Predicted_trip)
colnames(Predicted_trip)=c("TAZ","carpool_trips","pt_trips","taxi_trips","bike_trips","walk_trips","escooter_trips")

write.csv(Predicted_trip, file ="final_escooter_totalModeTrips_pSmart_onlyEscooter.csv")


# revenue for escooter
trip$escooter_revenue = trip$escooter_trips*(1+0.15*trip[19]/60)


final_es =cbind(trip$OTAZ,trip$p_escooter,trip$escooter_trips,trip$escooter_revenue)
colnames(final_es)=c("TAZ","escooter_prob","escooter_trip","escooter_revenue")





write.csv(final_es, file ="final_escooter_revenue_pSmart_onlyEscooter2.csv")



#distribute the revenue per zipcode  그냥 엑셀에서 하기

zip_taz = read.csv('TAZ_ZIPCODE_Percentage.csv')






