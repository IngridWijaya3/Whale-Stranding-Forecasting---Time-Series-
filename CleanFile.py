import pandas as pd
import numpy as np
import os
from itertools import combinations
from datetime import datetime
from datetime import date
import calendar
import seaborn as sns;
import pandas as pd
import matplotlib.pyplot as plt
sns.set(style="ticks", color_codes=True)

cwd = os.getcwd()+'/..'
filepath =cwd+"/original_file.xlsx"
xl = pd.ExcelFile(filepath)
df= xl.parse('all other species')
df=df[((df['Date_'].notnull()) & (df["Incident_type"]!="Accident")) ]
df['date']=pd.to_datetime(df["Date_"], format="%d/%m/%Y")
df=df.sort_values( ['date'], ascending=True)
print(df["date"].max())
datesIndex= pd.date_range(start=df["date"].min().strftime('%Y-%m-%d'), end='2018-01-01', freq='D')
df2 = pd.DataFrame({'date': datesIndex})
df2["No_Strand_1"]=0
mergedf=pd.merge(df2, df, on='date', how='left')
mergedf['No_Strand'].fillna(0, inplace=True)
mergedf['No_Strand'] = mergedf['No_Strand_1'] + mergedf['No_Strand']
mergedf=mergedf.drop(['No_Strand_1','LatLongs_Given','Date_Refloat','Refloat_Attempts','Time_Refloat','Refloat_Tech','No_Restrand','Time_Restrand','Date_Restrand','Loc_Restrand','AutopsyDetails'], axis=1)
#df = df.set_index('test')
mergedf.to_csv(cwd+'/fulldatefile.csv', date_format='%Y-%m-%d',encoding="utf-8",index=False)
onlylatlongdf=mergedf[["date","No_Strand","Lat","Long"]]
onlylatlongdf.to_csv(cwd+'/latlongdata.csv', date_format='%Y-%m-%d',encoding="utf-8",index=False)
#mergedf=mergedf[(mergedf["date"]>'1920-01-01')]
groupdf=mergedf.groupby(['date'])['No_Strand'].agg(['sum'])
#groupdfbiggerthan1=groupdf[(groupdf["count"]>1)]
groupdf.to_csv(cwd+'/combinecounts.csv', date_format='%Y-%m-%d',encoding="utf-8")
#groupdfbiggerthan800=groupdf[(groupdf["sum"]>800)]
#print(groupdfbiggerthan800)

#for name, group in groupByRegiondf:
groupByRegiondf=regiondf.groupby(['Region'])['No_Strand'].agg(['sum']).sort_values( ['sum'], ascending=False)

#groupdf.index=groupdf['date']
monthlycount=groupdf.groupby(pd.TimeGrouper(freq='M'))["sum"].sum()
print( monthlycount )
monthlycount.to_csv(cwd+'/combinecountsmonthly.csv', date_format='%Y-%m-%d',encoding="utf-8")
weeklycount = groupdf.groupby( pd.Grouper(freq='W-MON'))['sum'].sum()
#weeklycount = groupdf.reset_index()

#weeklycount=weeklycount.sort_values(['date'], ascending=True)
weeklycount.to_csv(cwd+'/combinecountsweekly.csv', date_format='%Y-%m-%d',encoding="utf-8",index=True)
print( weeklycount )
#yearlycount=mergedf.groupby(pd.TimeGrouper(freq='Y')).sum()
#print( yearlycount )

for name in groupByRegiondf.index:
    print(name)
    print(groupByRegiondf.loc[name])
    regiondf2= regiondf[(df["Region"]==name)]
    print(regiondf2["date"].min().strftime('%Y'))
    datesIndex2 = pd.date_range(start=regiondf2["date"].min().strftime('%Y')+"-01-01", end='2018-01-01', freq='D')
    df3 = pd.DataFrame({'date': datesIndex2})
    df3["No_Strand_1"] = 0
    mergedf2 = pd.merge(df3, regiondf2, on='date', how='left')
    mergedf2=mergedf2.sort_values(['date'], ascending=True)
    mergedf2['No_Strand'].fillna(0, inplace=True)
    mergedf2['No_Strand'] = mergedf2['No_Strand_1'] + mergedf2['No_Strand']
    mergedf2 = mergedf2.drop(['No_Strand_1'], axis=1)
    mergedf2 = mergedf2.groupby(['date'])['No_Strand'].agg(['sum'])

    mergedf2 = mergedf2.groupby(pd.Grouper(freq='W-MON'))['sum'].sum()
    #mergedf=mergedf2.sort_values(['date'], ascending=True)
    ##weeklycount.to_csv(cwd + '/combinecountsweekly.csv', date_format='%Y-%m-%d', encoding="utf-8")
    mergedf2.to_csv(cwd + '/'+name+'.csv', date_format='%Y-%m-%d', encoding="utf-8",index=True)

#sns.countplot(x='Region',y="sum", data=groupByRegiondf)
#groupByRegiondf.columns = ["Region","sum"]
groupByRegiondf=groupByRegiondf.reset_index()
print(groupByRegiondf)
#groupByRegiondf = groupByRegiondf[1:]
#sns.factorplot(x="Region", y="sum", kind="bar", data=groupByRegiondf)
# Rotate x-labels
#plt.xticks(rotation=-45)
#plt.interactive(False)
#plt.show()