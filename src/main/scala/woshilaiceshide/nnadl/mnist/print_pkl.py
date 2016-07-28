import mnist_loader
import numpy as np

training_data, validation_data, test_data =  mnist_loader.load_data_wrapper()

def ff(f): return "%.10f" % f

#def s(data): return [','.join([str(i[0]) for i in d[1].tolist()]) + "|" + ','.join([ff(i[0]) for i in d[0].tolist()]) for d in data]

def stdata(data):
 for d in data: 
  print ','.join([str(i[0]) for i in d[1].tolist()]) + "|" + ','.join([ff(i[0]) for i in d[0].tolist()])

def svdata(data):
 for d in data: 
  print str(d[1]) + "|" + ','.join([ff(i[0]) for i in d[0].tolist()])

#stdata(training_data)
#svdata(validation_data)
#svdata(test_data)
