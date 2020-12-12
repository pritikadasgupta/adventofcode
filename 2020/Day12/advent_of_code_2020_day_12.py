#!/bin/python3

#Completely written by my best friend, Jacob Waldman

import os

def change_direction(initial_direc, change_direc, degree):
    # Takes an initial direction, a change (direc) and number of degrees
    # Returns the new direction (N/S/E/W)
    assert initial_direc in ['N','S','E','W']
    assert degree % 90 == 0
    assert change_direc in ['L','R']
    all_direcs = ['N','E','S','W']
    deg_idx_change = degree/90
    if change_direc == 'L':
        index_change = (all_direcs.index(initial_direc)-deg_idx_change) % 4
    else:
        index_change = (all_direcs.index(initial_direc)+deg_idx_change) % 4        
    new_direc = all_direcs[index_change]

    assert new_direc in ['N','S','E','W']

    return new_direc

def move_direction(pos, direc, number):
    north_south_coord, east_west_coord = pos[0], pos[1]
    if direc == 'E':
        east_west_coord += number
    elif direc == 'W':
        east_west_coord -= number
    elif direc == 'S':
        north_south_coord -= number
    elif direc == 'N':
        north_south_coord += number
    else:
        raise Exception ('Should never happen!')

    return (north_south_coord, east_west_coord, pos[2])
        
def move_boat(pos, move):
    # Pos - initial position
    # Move - the move in the instruction set
    # Returns new position
    instruct_name, instruct_number = move[0], int(move[1:])
    if instruct_name in ['N','S','E','W']:
        new_pos = move_direction(pos, instruct_name, instruct_number)
        
    elif instruct_name in ['F']:
        heading = pos[2]
        new_pos = move_direction(pos, heading, instruct_number)

    elif instruct_name in ['L','R']:
        #print pos
        new_heading = change_direction(pos[2], instruct_name, instruct_number)
        new_pos = (pos[0], pos[1], new_heading)

    return new_pos

def calc_manhattan_dist(pos):
    return abs(pos[0]) + abs(pos[1])

def main_part1():
    in_file = open(os.environ['INPUT_PATH'],"r")
    #print in_file.readlines()
    pos = (0,0, 'E')
    for line in in_file:
        instruction = line.strip()
        #print instruction
        pos = move_boat(pos,instruction)
        print pos
    in_file.close()
    #print calc_manhattan_dist(pos)
    return

main_part1()
