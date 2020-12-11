import math
import copy
def read_input():
    input_arr = []
    with open("exercise11.input.txt") as i_f:
        for line in i_f:
            input_arr.append(list(line.rstrip()))
    return input_arr


def solve_puzzle(input_arr):
    while True:
        flag = True
        new_board = copy.deepcopy(input_arr)
        for j in range(0, len(input_arr)):
            for i in range(0, len(input_arr[j])):
                neighbours = []
                if i != 0:
                    neighbours.append(input_arr[j][i-1])
                if i != len(input_arr[j])-1:
                    neighbours.append(input_arr[j][i+1])
                if j != 0:
                    neighbours.append(input_arr[j-1][i])
                if j != len(input_arr)-1:
                    neighbours.append(input_arr[j+1][i])
                if i != 0 and j != 0:
                    neighbours.append(input_arr[j-1][i-1])
                if i != len(input_arr[j])-1 and j != len(input_arr)-1:
                    neighbours.append(input_arr[j+1][i+1])
                if i != 0 and j != len(input_arr)-1:
                    neighbours.append(input_arr[j+1][i-1])
                if i != len(input_arr[j])-1 and j != 0:
                    neighbours.append(input_arr[j-1][i+1])
                occupied_neighbours = neighbours.count("#")
                if input_arr[j][i] == "L" and occupied_neighbours == 0:
                    new_board[j][i] = "#"
                    flag = False
                elif input_arr[j][i] == "#" and occupied_neighbours >= 4:
                    new_board[j][i] = "L"
                    flag = False
        if flag == True:
            sm = 0
            for j in range(0, len(input_arr)):
                for i in range(0, len(input_arr[j])):
                    if input_arr[j][i] == "#":
                        sm += 1
            return sm
        input_arr = new_board

def solve_puzzle2(input_arr):
    num_steps = 0
    while True:
        num_steps += 1
        for line in input_arr:
            print(line)
        print()
        flag = True
        new_board = copy.deepcopy(input_arr)
        for j in range(0, len(input_arr)):
            for i in range(0, len(input_arr[j])):
                neighbours2 = []
                jj = j
                while True:
                    jj += 1
                    if jj >= len(input_arr):
                        break
                    if input_arr[jj][i] in ["#", "L"]:
                        neighbours2.append(input_arr[jj][i])
                        break
                jj = j
                while True:
                    jj -= 1
                    if jj < 0:
                        break
                    if input_arr[jj][i] in ["#", "L"]:
                        neighbours2.append(input_arr[jj][i])
                        break
                ii = i
                while True:
                    ii += 1
                    if ii >= len(input_arr[j]):
                        break
                    if input_arr[j][ii] in ["#", "L"]:
                        neighbours2.append(input_arr[j][ii])
                        break
                ii = i
                while True:
                    ii -= 1
                    if ii < 0:
                        break
                    if input_arr[j][ii] in ["#", "L"]:
                        neighbours2.append(input_arr[j][ii])
                        break
                ii = i
                jj = j
                while True:
                    ii += 1
                    jj += 1
                    if ii >= len(input_arr[j]) or jj >= len(input_arr):
                        break
                    if input_arr[jj][ii] in ["#", "L"]:
                        neighbours2.append(input_arr[jj][ii])
                        break
                ii = i
                jj = j
                while True:
                    ii -= 1
                    jj += 1
                    if ii < 0 or jj >= len(input_arr):
                        break
                    if input_arr[jj][ii] in ["#", "L"]:
                        neighbours2.append(input_arr[jj][ii])
                        break
                ii = i
                jj = j
                while True:
                    ii -= 1
                    jj -= 1
                    if ii < 0 or jj < 0:
                        break
                    if input_arr[jj][ii] in ["#", "L"]:
                        neighbours2.append(input_arr[jj][ii])
                        break
                ii = i
                jj = j
                while True:
                    ii += 1
                    jj -= 1
                    if ii >= len(input_arr[j]) or jj < 0:
                        break
                    if input_arr[jj][ii] in ["#", "L"]:
                        neighbours2.append(input_arr[jj][ii])
                        break
                #for jj in range(0, len(input_arr)):
                #    for ii in range(0, len(input_arr)):
                #        if input_arr[jj][ii] == "." or (jj,ii) == (j,i):
                #            continue
                #        dj = jj - j
                #        di = ii - i
                #        angle = math.atan2(dj, di)
                #        if angle not in neighbours:
                #            neighbours[angle] = input_arr[jj][ii]
                #occupied_neighbours = sum(list(map(lambda l : l.count("#"), neighbours.values())))
                occupied_neighbours = neighbours2.count("#")
                if input_arr[j][i] == "L" and occupied_neighbours == 0:
                    new_board[j][i] = "#"
                    flag = False
                elif input_arr[j][i] == "#" and occupied_neighbours >= 5:
                    new_board[j][i] = "L"
                    flag = False
        if flag == True:
            sm = 0
            for j in range(0, len(input_arr)):
                for i in range(0, len(input_arr[j])):
                    if input_arr[j][i] == "#":
                        sm += 1
            return sm
        input_arr = new_board


def main():
    input_arr = read_input()
    print(solve_puzzle(input_arr))
    print(solve_puzzle2(input_arr))

main()
