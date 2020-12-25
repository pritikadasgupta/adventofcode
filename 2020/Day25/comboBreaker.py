import re

a = 6270530
b = 14540258

x = 1
lsz = 0
while x != a:
  x *= 7
  x %= 20201227
  lsz += 1
print(lsz)

x = 1
for _ in range(lsz):
  x *= b
  x %= 20201227
print(x)