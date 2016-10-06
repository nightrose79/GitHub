# -*- coding: utf-8 -*-
"""
Created on Tue Feb 16 12:58:21 2016

@author: yuy
"""

#from pyalgotrade.feed import csvfeed
from pyalgotrade.barfeed import ninjatraderfeed

feed = ninjatraderfeed.Feed(ninjatraderfeed.Frequency.MINUTE)
#feed.addVBarsFromCSV("C:\\Users\\yuy\\Documents\\GitHub\\try-master\\try-master\\cubist\\data.1_min.csv")
#        feed = ninjatraderfeed.Feed(ninjatraderfeed.Frequency.MINUTE)        
feed.addBarsFromCSV("cub", "C:\\Users\\yuy\\Documents\\GitHub\\try-master\\try-master\\cubist\\data.1_min.csv")
for dateTime, value in feed:
    print dateTime, value

#df=pd.read_csv('data.1_min.csv',header=None,names=('time','price'))
#df.head()