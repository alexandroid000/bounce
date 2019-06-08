#!/usr/bin/python
import os
import time
def frange(start, stop, step):
  i = start
  while i < stop:
    yield i
    i += step

iters = [10, 800]
resolution = 0.001

# for s_pos in frange(0.48, 0.49, resolution):
#   for n_iter in iters:
#     os.system('stack exec -- bounce-exe -o tworefsym/tworefsym_{}_{}_{}.svg -n {} -e tworefsym -b Fixed -a 0.80 -s {}'.format(time.time(), n_iter, s_pos, n_iter, s_pos))

for i in range(25):
    os.system('stack exec -- bounce-exe -o tworefsym_shear/tworefsym_{}.svg -n 300 -e tworefsym_{} -b Fixed -a 0.80 -s 0.45'.format(i, i))
