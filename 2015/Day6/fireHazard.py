import re
 
lightMatrix = [[0 for x in range(1000)] for x in range(1000)] 
brightnessCounter = 0
 
with open('input.txt') as f:
  for line in f:
    # Match command
    res = re.search(r'(turn on|turn off|toggle)\s*(\d*?),(\d*?)\s*through\s(\d*?),(\d*)', line)
    command, lower_X, lower_Y, upper_X, upper_Y =  res.group(1), int(res.group(2)), int(res.group(3)), int(res.group(4)), int(res.group(5))
    
    # Perform actions
    if command == 'turn off':
      data = -1
    elif command == 'turn on':
      data = 1
    elif command == 'toggle':
      data = 2
      
    # Apply action
    for x_val in range (lower_X, upper_X + 1):
      for y_val in range (lower_Y, upper_Y + 1):
        lightMatrix[x_val][y_val] += data if lightMatrix[x_val][y_val] + data >= 0 else 0
        
# Get brightness level
for i in lightMatrix:
  for brightness in i:
    brightnessCounter += brightness
 
# answer
print(brightnessCounter)