commands = [(line[:1], int(line[1:]))
    for line in open("exercise12.input.txt").readlines()]

dirs = { 'N':(0, 1), 'S':(0, -1), 'W':(1, 0), 'E':(-1, 0) }
rot =  { 'R':(-1, 1), 'L': (1, -1) }

def execute(command, value, strafe=True):
    global x, y, wx, wy
    if strafe and command in dirs:  # Strafing moves the ship
        x += dirs[command][0] * value
        y += dirs[command][1] * value
    if not strafe and command in dirs:  # Othervise move waypoint
        wx += dirs[command][0] * value
        wy += dirs[command][1] * value
    if command in rot:
       for i in range(value // 90):  # Rotate waypoint multiple times
           wx, wy = wy * rot[command][0], wx * rot[command][1]
    if command == 'F':  # Move ship in waypoint direction
        x += wx * value
        y += wy * value

### Part 1
x, y   = 0, 0
wx, wy = -1, 0  # Start by facing east
for command, value in commands:
    execute(command, value, strafe=True)
distance = abs(x) + abs(y)
print(distance)


### Part 2
x, y = 0, 0
wx, wy = -10, 1  # Waypoint starts 10E 1N
for command, value in commands:
    execute(command, value, strafe=False)
distance_correct = abs(x) + abs(y)
print(distance_correct)