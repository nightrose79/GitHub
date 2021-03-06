# -*- coding: utf-8 -*-
"""
Created on Sun Feb 14 17:16:23 2016

@author: nightrose
"""

import numpy
import talib

close = numpy.random.random(100)
#Calculate a simple moving average of the close prices:

output = talib.SMA(close)
#Calculating bollinger bands, with triple exponential moving average:

from talib import MA_Type

upper, middle, lower = talib.BBANDS(close, matype=MA_Type.T3)

import matplotlib as plt
plt.plot(upper)
#Calculating momentum of the close prices, with a time period of 5:

output = talib.MOM(close, timeperiod=5)