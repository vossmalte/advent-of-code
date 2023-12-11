import numpy as np


def prettyPrint(string, color="91"):
    return "\033[" + color + "m" + string + "\033[0m"


def linesToNpArray(lines: list[str]) -> np.array:
    return np.array(
        [
            [prettyPrint(c, "90") if c == "." else c + "" for c in list(line)]
            for line in lines
        ]
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


def main(lines):
    arr = linesToNpArray(lines)
    start = np.argwhere(arr == "S")[0]
    currentLocation, currentDirection = findFirstPipe(arr, start)
    pipe = ""
    steps = 0
    while True:
        steps += 1
        pipe = arr[currentLocation[0]][currentLocation[1]]
        arr[currentLocation[0]][currentLocation[1]] = prettyPrint(pipe)
        # print(str(arr))

        if pipe == "S":
            break

        currentDirection = getNextDirection(-currentDirection, pipe)
        currentLocation += currentDirection
    arr[start[0]][start[1]] = prettyPrint("S", "43")
    printArray(arr)
    return steps / 2


if __name__ == "__main__":
    if True:
        # print(parseDraw("3 green, 3 blue"))
        pass
    # test
    with open("input-first.txt") as file:
        result = main(file.readlines())
        if not result == 8:
            print("first test failed")
    # with open("input-second.txt") as file:
    #     result = main2(file.readlines())
    #     if not result == 2286:
    #         print("second test failed")
    #
    with open("input.txt") as file:
        result1 = main(file.readlines())
        # result2 = main2(file.readlines())

        print(result1)
    #     pass
