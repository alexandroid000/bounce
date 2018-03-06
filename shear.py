#!/usr/bin/python

import numpy as np

pts_input = np.array([np.array(x) for x in [(800, 0), (600, 300), (600, 500), (800, 800), (0, 400)]])

pts_output = []

def get_trans(shx, shy):
  shear_transformation = np.array([[1, shy],[shx, 1]])
  return shear_transformation

def app_trans(trans, pts):
  output = []
  for pt in pts:
    output.append(np.matmul(trans, np.reshape(pt, (2, 1))))
  return output

name = 'tworefsym'
if __name__ == '__main__':
  count = 0
  for shx in np.arange(0., 1., 0.2):
    for shy in np.arange(0., 1., 0.2):
      trans = get_trans(shx, shy)
      output = app_trans(trans, pts_input)
      output = list(tuple([int(x) for x in list(y)]) for y in output)
      output_name = '{}_{}'.format(name, count)
      print('{} = Pts {}'.format(output_name, output))
      count += 1
  count = 0
  for shx in np.arange(0., 1., 0.2):
    for shy in np.arange(0., 1., 0.2):
      output_name = '{}_{}'.format(name, count)
      print('("{}", {}),'.format(output_name, output_name))
      count += 1
