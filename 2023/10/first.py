import numpy as np


def prettyPrint(string, color="91"):
    return "\033[" + color + "m" + string + "\033[0m"


def linesToNpArray(lines: list[str]) -> np.array:
    return np.array(
        [[c + "" if c == "." else c + "" for c in list(line)] for line in lines]
    )


N = np.array([-1, 0])
E = np.array([0, 1])
S = np.array([1, 0])
W = np.array([0, -1])
DIRECTIONS = np.array([N, E, S, W])

pipes = {
    "|": (S, N),
    "-": (W, E),
    "7": (W, S),
    "F": (E, S),
    "J": (N, W),
    "L": (N, E),
    ".": (np.array([0, 0]), np.array([0, 0])),
}


def getNextDirection(fromDirection, pipe: str):
    nextDirA, nextDirB = pipes.setdefault(pipe, (np.array([0, 0]), np.array([0, 0])))
    if np.all(fromDirection == nextDirA):
        return nextDirB
    if np.all(fromDirection == nextDirB):
        return nextDirA
    # return nextDirA
    raise ValueError("pipe " + pipe + " cannot be connected from " + str(fromDirection))


def findFirstPipe(arr, start):
    for d in DIRECTIONS:
        currentLocation = start + d
        next = arr[currentLocation[0]][currentLocation[1]]
        nextDirA, nextDirB = pipes.setdefault(
            next, (np.array([0, 0]), np.array([0, 0]))
        )
        if np.all(-d == nextDirA) or np.all(-d == nextDirB):
            return (start + d, d)
    return (np.array([-1, -1]), np.array([0, 0]))


def printArray(arr):
    for line in arr:
        for c in line:
            print(c, end="")
        print()


def findLoopIndices(arr):
    a = arr.copy()
    start = np.argwhere(a == "S")[0]
    currentLocation, currentDirection = findFirstPipe(a, start)
    pipe = ""
    while True:
        pipe = a[currentLocation[0]][currentLocation[1]]
        a[currentLocation[0]][currentLocation[1]] = "X"

        if pipe == "S":
            break

        currentDirection = getNextDirection(-currentDirection, pipe)
        currentLocation += currentDirection
    indices = np.nonzero(a == "X")
    return indices


def growRegion(arr, loopIndicesList, start, replaceWith):
    [maxH, maxW] = arr.shape
    nodesToEvaluate = [start]
    while len(nodesToEvaluate) != 0:
        node = nodesToEvaluate.pop()
        if node.tolist() in loopIndicesList or arr[node[0]][node[1]] == replaceWith:
            continue
        arr[node[0]][node[1]] = replaceWith
        for d in DIRECTIONS:
            next = node + d
            if (
                np.all(next >= np.array([0, 0]))
                and np.all(next < np.array([maxH, maxW]))
                and arr[next[0]][next[1]] != replaceWith
            ):
                nodesToEvaluate.append(next)


def main2(lines, regionSeed=[0, 0]):
    a = linesToNpArray(lines)

    loopIndices = findLoopIndices(a)
    arr = a.copy()
    arr[np.any(arr != "z")] = "."
    arr[loopIndices] = a[loopIndices]
    loopIndicesList = np.transpose(loopIndices).tolist()
    growRegion(arr, loopIndicesList, np.array(regionSeed), " ")

    # arr[loopIndices] = " "
    # arr[regionSeed[0]][regionSeed[1]]='X'
    printArray(arr)
    return np.count_nonzero(arr == "I")


def main1(lines):
    arr = linesToNpArray(lines)
    loopIndices = findLoopIndices(arr)
    return len(loopIndices[1]) / 2


if __name__ == "__main__":
    if True:
        # print(parseDraw("3 green, 3 blue"))
        pass
    # test
    with open("input-first.txt") as file:
        result = main1(file.readlines())
        if not result == 8:
            print("first test failed")

    with open("input-second.txt") as file:
        result = main2(file.readlines())
        if not result == 4:
            print("second test failed")

    with open("input.txt") as file:
        lines = file.readlines()
        result1 = main1(lines)
        result2 = main2(lines, [72, 79])

        print("puzzle 1", result1)
        print("puzzle 1", result2)
    #     pass
