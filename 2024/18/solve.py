import numpy as np
from importlib import reload

prod = False
prod = True

fileName = "input.txt" if prod else "input-test.txt"
maxCoord = 70 if prod else 6
fallenBytes = 1024 if prod else 12

arr = np.zeros((maxCoord+1, maxCoord+1), dtype=np.dtype("<i4"))

(maxX, maxY) = arr.shape

input = np.loadtxt(
    fileName,
    dtype=np.dtype("<i4"),
    delimiter=",",
)

for i in range(fallenBytes):
    [x, y] = input[i]
    arr[y, x] = 1


# from my memory
def dijkstra(start=(0,0), end=(maxCoord, maxCoord)):
    currentLocation = start
    visited = []
    toVisit = [start]
    parent = {}
    while len(toVisit) > 0:
        currentLocation = toVisit.pop(0)
        visited.append(currentLocation)
        nextLocations = list(
            filter(lambda l: l not in visited and l not in toVisit, getNextLocations(currentLocation))
        )
        toVisit.extend(nextLocations)
        for n in nextLocations:
            parent.update({n: currentLocation})
        if currentLocation == end:
            break
    else:
        raise Exception("The end has not been found")
    return parent


def resolve_steps(d, start=(0,0), end=(maxCoord, maxCoord)):
    a = arr.copy()
    path = [end]
    current = end
    while not current == start:
        a[current] = 2
        current = d.get(current)
        path.append(current)
    np.savetxt("output.txt", a, fmt="%d")
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
            lambda l: arr[(l)] == 0,  # not corrupted
            (
                filter(
                    isValidLocation,
                    map(lambda d: tuple(np.array(location) + d), directions),
                )
            ),
        )
    )


def solve1():
    print( "solve1:", len( resolve_steps( dijkstra()             ))-1)

def solve2():
    for i in range(fallenBytes, len(input)):
        [x, y] = input[i]
        arr[y, x] = 1
        print(i, (x,y))
        try:
            dijkstra((0, 0), (maxCoord, maxCoord))
        except:
            print(" ^ this one broke it")
            break

if __name__ == '__main__':
    solve1()
