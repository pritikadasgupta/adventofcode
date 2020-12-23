import re
 
nice_counter = 0
improved_nice_counter = 0
naughty_list = ['ab', 'cd', 'pq', 'xy']
 
def nice_string(str):
  if len(re.sub(r'[^aeiou]', '', str)) < 3:
    return False
 
  # Appear twice
  if not re.search(r'(.)\1', str):
    return False
 
  # No naughty words
  for nw in naughty_list:
    if nw in str:
      return False
 
  return True
 
def improved_nice_string(str):
  # Check for double pair
  if not re.search(r'(.)(.).*\1\2', str): #some wild boobies appear
    return False
    
  # letter repeat
  if not re.search(r'(.).\1', str):
    return False
    
  return True
      
      
with open('input.txt') as f:
  for line in f:
    
    if nice_string(line):
      nice_counter += 1
      
    if improved_nice_string(line):
      improved_nice_counter += 1
 
 
# answer
print(nice_counter)
print(improved_nice_counter)