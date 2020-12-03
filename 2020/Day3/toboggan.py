#!/bin/python3

import os

# How many passwords are valid according to their policies?
def tobaggan(ar,right,down):
	#start at top-left corner
	idx=0
	trees=0
	for x in range(0,len(ar),down):
		if(ar[x][idx]=='.'):
			trees=trees+0
		elif(ar[x][idx]=='#'):
			trees=trees+1
		idx = (idx+right)%len(ar[x])
	return trees


with open(os.environ['INPUT_PATH']) as ar_count: 
	ar = [x for x in ar_count.read().split()]

#part 1
tobaggan(ar,3,1)


#part 2
tobaggan(ar,1,1)*tobaggan(ar,3,1)*tobaggan(ar,5,1)*tobaggan(ar,7,1)*tobaggan(ar,1,2)