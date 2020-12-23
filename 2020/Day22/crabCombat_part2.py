def p1_wins(p1, p2, part2):
    prev_rounds = [(p1.copy(), p2.copy())]

    while len(p1) > 0 and len(p2) > 0:
        val1 = p1.pop(0)
        val2 = p2.pop(0)
        if part2 and len(p1) >= val1 and len(p2) >= val2:
            if p1_wins(p1[0:val1].copy(), p2[0:val2].copy(), True):
                p1.append(val1)
                p1.append(val2)
            else:
                p2.append(val2)
                p2.append(val1)
        else:
            if val1 > val2:
                p1.append(val1)
                p1.append(val2)
            else:
                p2.append(val2)
                p2.append(val1)
        if part2 and (p1, p2) in prev_rounds:
            return True
        elif part2:
            prev_rounds.append((p1.copy(), p2.copy()))

    if len(p1) > len(p2):
        return True
    else:
        return False


def score(p):
    count = 0
    for i, val in enumerate(p):
        count += val * (len(p) - i)
    return count


with open("input.txt") as f:
    lines = f.readlines()

p1 = []
for i in range(25):
    p1.append(int(lines[i+1]))

p2 = []
for i in range(25):
    p2.append(int(lines[i+28]))

p1_copy = p1.copy()
p2_copy = p2.copy()

if p1_wins(p1, p2, False):
    print("Player 1 won first game with score:", score(p1))
else:
    print("Player 2 won first game with score:", score(p2))

if p1_wins(p1_copy, p2_copy, True):
    print("Player 1 won second game with score:", score(p1_copy))
else:
    print("Player 2 won second game with score:", score(p2_copy))
