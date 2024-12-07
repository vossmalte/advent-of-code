import numpy as np
from importlib import reload

fileName = "input.txt"
# fileName = "input-test.txt"

# format:
# %s/\(.\)/\1 /g
# %s/ $//g
# %s/#/O/g

OBSTACLE = "O"
GUARD = "^"
TRACE = "X"

input = np.loadtxt(
    fileName,
    dtype=np.dtype("<U1"),
    delimiter=" ",
)

# rotate clockwise so walking direction is to the right
arrR = np.rot90(input, -1).copy()


def walk_and_mark_and_rotate(arr, s=set(), r=0):
    [x, y] = np.argwhere(arr == GUARD)[0]
    # what the guard is seeing
    next_obstacles = np.argwhere(arr[x, y:] == OBSTACLE)
    if next_obstacles.size == 0:
        arr[x, y:].fill(TRACE)
        # print("Guard left the area")
        return arr

    next_obstacle = np.min(next_obstacles)

    if (x, y, r % 4) in s:
        # print("Guard is in a loop")
        return True
    else:
        s.add((x, y, r % 4))

    # leave a trace
    arr[x, y : y + next_obstacle].fill(TRACE)

    # mark position
    arr[x, y + next_obstacle - 1] = GUARD

    # rotate
    arr = np.rot90(arr, 1)
    return walk_and_mark_and_rotate(arr, s, r + 1)


print("Part 1:", np.count_nonzero(walk_and_mark_and_rotate(arrR.copy()) == TRACE))


def check_new_obstacle_creates_loop(arr, pos):
    [x, y] = np.argwhere(arr == GUARD)[0]
    start = (x, y)
    if start == pos:
        return False
    scenario = arr.copy()
    scenario[pos] = OBSTACLE
    return bool(np.all(walk_and_mark_and_rotate(scenario, set()) == True))


def countLoopScenarios(arr):
    positions = np.argwhere(walk_and_mark_and_rotate(arr.copy(), set()).base == TRACE)
    counter = 0
    for p in positions:
        [x, y] = p
        counter += check_new_obstacle_creates_loop(arr.copy(), (x, y))
    return counter


print("Part 2:", countLoopScenarios(arrR))
