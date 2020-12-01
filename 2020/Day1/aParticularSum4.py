#!/bin/python3

import math
import os
import random
import re
import sys


#Find THREE entries in "ar" that sum to "k"
#What do you get when you multiply them together?
def aParticularSum(ar,k):
    multiple = 0
    ar = sorted(ar)
    for x in range(0, len(ar)-2):   
        y = x + 1 
        z = len(ar)-1 
        while (y < z): 
            if(ar[x] + ar[y] + ar[z] == k): 
                multiple = ar[x] * ar[y] * ar[z]
                return multiple
            elif (ar[x] + ar[y] + ar[z] < k): 
                y += 1
            else:
                z -= 1  
    return False


fptr = open(os.environ['OUTPUT_PATH'], 'w')
with open(os.environ['INPUT_PATH']) as ar_count:
    ar = [int(x) for x in ar_count.read().split()]
k = int(input("What do you want your sum to be? ")) #What do you want your sum to be?
result = aParticularSum(ar,k)
print(result)
fptr.write(str(result) + '\n')
fptr.close()