from itertools import product
 
X = Y = 0
visited = [(0,0)] #(0,0) always visited
unique_houses_visited = 1 #house at (0,0)
 
with open('input.txt') as f:
  for c in f.read():
    if c == '^':
      Y += 1
    elif c == '<':
      X -= 1
    elif c == 'v':
      Y -= 1
    elif c == '>':
      X += 1
      
    # Check if new house
    if (X, Y) not in visited:
      # New house
      unique_houses_visited += 1
      visited.append((X, Y))
 
#answer      
#print "Unique houses visited:", unique_houses_visited
 
santaX = santaY = 0
roboX = roboY = 0
visited = [(0,0)] #(0,0) always visited
unique_houses_visited = 1 #house at (0,0) visited by Santa and Robo-santa
santasTurn = True
 
def is_new_house(X, Y, visited):
  if (X, Y) not in visited:
    # New house
    visited.append((X, Y))
    return 1
  return 0
 
with open('input.txt') as f:
  for c in f.read():
    if c == '^':
      santaY += int(santasTurn)
      roboY += int(not santasTurn)
    elif c == '<':
      santaX -= int(santasTurn)
      roboX -= int(not santasTurn)
    elif c == 'v':
      santaY -= int(santasTurn)
      roboY -= int(not santasTurn)
    elif c == '>':
      santaX += int(santasTurn)
      roboX += int(not santasTurn)
 
    # Check if new house
    if santasTurn:
      unique_houses_visited += is_new_house(santaX, santaY, visited)
    else:
      unique_houses_visited += is_new_house(roboX, roboY, visited)
    
    # Swap turns!
    santasTurn = not santasTurn
 
#answer      
print(unique_houses_visited)