import numpy as np
class TimeSeries_WF():
    
    def __init__(self,n,training,testing):
        self.n=n
        self.testing=testing
        self.training=training
        
    def __iter__(self):
#        if self.n<(self.fold+1):
#            print("too many folds")
        m=(self.n-self.training)%(self.testing) #if there are leftover observations, add to the first training set
        print('m is',m)
        series=np.arange(self.n)#entire series
        fold=(self.n-self.training-m)/(self.testing)
# size=(self.n-m)/(self.fold+1)#size of each fold, size of the first fold is size+m
        for i in np.arange(fold):
            if i==0:
                train_index = series[0:(m+self.training)]
            else:
                train_index = series[(self.testing*i+m):(self.testing*(i)+m+self.training)]
            test_index = series[(self.testing*(i)+m+self.training):(self.testing*(i+1)+m+self.training)]
            yield train_index, test_index
    def __len__(self):
        return self.fold 