#!/bin/python3

import math
import os
import random
import re
import sys


#Time Complexity: O(nlog(n))
#Space Complexity: O(1)

#Find two entries in "ar" that sum to "k"
#What do you get when you multiply them together?
def aParticularSum(ar,k):
    multiple = 0
    ar = sorted(ar) # sort the list ar
    minidx = 0 # min index
    maxidx = len(ar) - 1 #max index
    while(minidx < maxidx):
        if(ar[minidx] + ar[maxidx] == k):
            multiple = ar[minidx] * ar[maxidx]
        if(ar[minidx] + ar[maxidx] < k):
            minidx +=1
        else:
            maxidx -=1
    return multiple


fptr = open(os.environ['OUTPUT_PATH'], 'w')
with open(os.environ['INPUT_PATH']) as ar_count:
    ar = [int(x) for x in ar_count.read().split()]
k = int(input("What do you want your sum to be? ")) #What do you want your sum to be?
result = aParticularSum(ar,k)
print(result)
fptr.write(str(result) + '\n')
fptr.close()