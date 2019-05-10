import pandas as pd


citi8 = pd.read_csv('/Users/mina/Dropbox/2019Spring/Transportation_Behavioral_Informatics/TBI_project/Data/citibike/201808-citibike-tripdata.csv')
citi9 = pd.read_csv('/Users/mina/Dropbox/2019Spring/Transportation_Behavioral_Informatics/TBI_project/Data/citibike/201809-citibike-tripdata.csv')
citi10 = pd.read_csv('/Users/mina/Dropbox/2019Spring/Transportation_Behavioral_Informatics/TBI_project/Data/citibike/201810-citibike-tripdata.csv')
citi11 = pd.read_csv('/Users/mina/Dropbox/2019Spring/Transportation_Behavioral_Informatics/TBI_project/Data/citibike/201811-citibike-tripdata.csv')

citi_total=pd.concat([ citi8,citi9, citi10, citi11])
citi_total_station=citi_total[['start station id','start station latitude','start station longitude']]


tripCountPerStation=citi_total.groupby('start station id').count()
tripCountPerStation=tripCountPerStation.apply(lambda x: x)


result = tripCountPerStation.merge(citi_total_station, left_on='start station id', right_on='start station id')
result=result[['start station id','start station latitude_y','start station longitude_y','starttime']]
result.columns=['s_id','s_lat','s_long','numOfTrip']
result.drop_duplicates(subset='s_id', keep='first')

result.to_csv(index=False)