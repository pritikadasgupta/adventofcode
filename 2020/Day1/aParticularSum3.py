#!/bin/python3

import math
import os
import random
import re
import sys


#Time Complexity: O(n^3)

#Find THREE entries in "ar" that sum to "k"
#What do you get when you multiply them together?
def aParticularSum(ar,k):
    multiple = 0
    for x,xval in enumerate(ar,start=0):
        ar_y = ar[:x] + ar[x+1:]
        for y,yval in enumerate(ar_y,start=0):
            ar_z = ar_y[:y] + ar_y[y+1:]
            for z,zval in enumerate(ar_z,start=0):
                if(xval+yval+zval == k):
                    multiple = xval*yval*zval
    return multiple


fptr = open(os.environ['OUTPUT_PATH'], 'w')
with open(os.environ['INPUT_PATH']) as ar_count: 
    ar = [int(x) for x in ar_count.read().split()]
k = input("What do you want your sum to be? ")
result = aParticularSum(ar,k)
print(result)
fptr.write(str(result) + '\n')
fptr.close()