#!/bin/python3

import math
import os
import random
import re
import sys


#Find two entries in "ar" that sum to "k"
#What do you get when you multiply them together?
def aParticularSum(ar,k):
    return ({x*(k-x) for x in ar if (k-x) in ar})


fptr = open(os.environ['OUTPUT_PATH'], 'w')
with open(os.environ['INPUT_PATH']) as ar_count:
    ar = [int(x) for x in ar_count.read().split()]
k = int(input("What do you want your sum to be? ")) #What do you want your sum to be?
result = aParticularSum(ar,k)
print(result)
fptr.write(str(result) + '\n')
fptr.close()