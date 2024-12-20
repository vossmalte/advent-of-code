import numpy as np
from collections import Counter
from importlib import reload

prod = False
prod = True

fileName = "input.txt" if prod else "input-test.txt"


arr = np.loadtxt(
    fileName,
    dtype=np.dtype("<U1"),
    delimiter=" ",
)

(maxX, maxY) = arr.shape

start = tuple(np.argwhere(arr == "S")[0])
end = tuple(np.argwhere(arr == "E")[0])

arr[start] = "."
arr[end] = "."


# see 2024/18
# from my memory
def dijkstra(start=start, end=end):
    currentLocation = start
    visited = []
    toVisit = [start]
    parent = {}
    while len(toVisit) > 0:
        currentLocation = toVisit.pop(0)
        visited.append(currentLocation)
        nextLocations = list(
            filter(
                lambda l: l not in visited and l not in toVisit,
                getNextLocations(currentLocation),
            )
        )
        toVisit.extend(nextLocations)
        for n in nextLocations:
            parent.update({n: currentLocation})
        if currentLocation == end:
            break
    else:
        raise Exception("The end has not been found")
    print("dijkstra done")
    return parent


def resolve_steps(d, start=start, end=end):
    path = [end]
    current = end
    while not current == start:
        current = d.get(current)
        path.append(current)
    print("resolve_steps done")
    return path


# from 2024/10
def isValidLocation(location):
    "check if location is valid"
    return (
        location[0] >= 0
        and location[0] < maxX
        and location[1] >= 0
        and location[1] < maxY
    )


# from 2024/10
directions = [[0, 1], [1, 0], [0, -1], [-1, 0]]


def getNextLocations(location):
    "return all locations that are one higher from the location"
    return list(
        filter(
            lambda l: arr[l] == ".",  # not a wall
            (
                filter(
                    isValidLocation,
                    map(lambda d: tuple(np.array(location) + d), directions),
                )
            ),
        )
    )


def getCheats(path, stepLimit=2):
    "return all pairs of locations where a cheat can be applied and return its saving"
    p = list(map(np.array, path))
    cheats = []
    for i in range(len(p)):
        for j in range(i + 1, len(p)):
            diff = p[j] - p[i]
            steps = np.sum(np.abs(diff))
            if steps <= stepLimit and j-i > steps:
                # print("cheat",i,j,i-j)
                cheats.append((p[i], p[j], i, j, j - i - steps))
    return cheats

def solve1():
    cheats = getCheats(resolve_steps(dijkstra()))
    counter = Counter(list(map(lambda c: c[4], cheats)))
    d = counter - Counter(dict([(i, 1000) for i in range(100)]))
    print("solve1:", d.total())
    return counter

def solve2():
    cheats = getCheats(resolve_steps(dijkstra()), stepLimit=20)
    counter = Counter(list(map(lambda c: c[4], cheats)))
    d = counter - Counter(dict([(i, 100000) for i in range(100)]))
    print("solve2:", d.total())
    return counter


if __name__ == "__main__":
    solve1()
    solve2()
