from heapq import heappop, heappush

with open("input.txt") as f:
    mydata = [list(map(int, line)) for line in f.read().strip().split("\n")]


def low_risk_score(t):
    heap = [(0, 0, 0)]
    seen = {(0, 0)}
    while heap:
        distance, x, y = heappop(heap)
        if x == t * len(mydata) - 1 and y == t * len(mydata[0]) - 1:
            return distance
        for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            x_, y_ = x + dx, y + dy
            if x_ < 0 or y_ < 0 or x_ >= t * len(mydata) or y_ >= t * len(mydata):
                continue
            a, am = divmod(x_, len(mydata))
            b, bm = divmod(y_, len(mydata[0]))
            n = ((mydata[am][bm] + a + b) - 1) % 9 + 1
            if (x_, y_) not in seen:
                seen.add((x_, y_))
                heappush(heap, (distance + n, x_, y_))

# PART 1
print(low_risk_score(1))
# PART 2
print(low_risk_score(5))