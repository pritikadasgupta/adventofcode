#!/bin/python3

import re
import os

bags = {}

with open(os.environ['INPUT_PATH']) as file:
	for line in file:
		bag, tobag = line.strip().split(" bags contain ")
		bags[bag] = []
		if (tobag == "no other bags."):
			continue
		for tobagbag in [bag.strip() for bag in tobag.split(',')]:
			num, type = re.match(r'(\d+) (.+) bag.*',tobagbag).groups()
			bags[bag].append((type,int(num)))

def part1(bag):
	return bag == "shiny gold" or any(part1(tobag) for tobag, _ in bags[bag])

def part2(bag):
	return 1+ sum(n*part2(tobag) for tobag,n in bags[bag])

#part 1
print("Part 1: ",sum(part1(bag) for bag in bags) - 1)

#part 2
print("Part 1: ",part2("shiny gold") - 1)