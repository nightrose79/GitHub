# -*- coding: utf-8 -*-
"""
Created on Wed Aug 31 10:47:30 2016

@author: yuy
"""

from sklearn import svm, grid_search, datasets
from spark_sklearn import GridSearchCV

iris = datasets.load_iris()
parameters = {'kernel':('linear', 'rbf'), 'C':[1, 10]}
svr = svm.SVC()
clf = GridSearchCV(sc, svr, parameters)
clf.fit(iris.data, iris.target)