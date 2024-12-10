import numpy as np
from importlib import reload

fileName = "input-test.txt"
fileName = "input.txt"

input = np.loadtxt(
    fileName,
    dtype=np.dtype(">i4"),
    delimiter=" ",
)


(maxX, maxY) = input.shape
maxSize = max(maxX, maxY)

directions = [[0, 1], [1, 0], [0, -1], [-1, 0]]


def isValidLocation(location):
    "check if location is valid"
    return (
        location[0] >= 0
        and location[0] < maxX
        and location[1] >= 0
        and location[1] < maxY
    )


def getNextLocations(location):
    "return all locations that are one higher from the location"
    return list(
        filter(
            lambda l: input[tuple(l)] == 1 + input[tuple(location)],
            (
                filter(
                    isValidLocation, map(lambda d: np.array(location) + d, directions)
                )
            ),
        )
    )


def getPeaksFrom(location):
    "start at location and return peaks that are reachable"
    if input[tuple(location)] == 9:
        return [location]
    result = []
    for nextLocation in getNextLocations(location):
        result.extend(getPeaksFrom(nextLocation))
    return result

def trailheadScore(location):
    "return the score for a trailhead"
    return len(list(np.unique(getPeaksFrom(location),axis=0)))

def trailheadScore2(location):
    "return the score for a trailhead"
    return len(getPeaksFrom(location))

def solve1():
    trailheads = list(np.argwhere(input == 0))
    return sum(map(trailheadScore, trailheads))

def solve2():
    trailheads = list(np.argwhere(input == 0))
    return sum(map(trailheadScore2, trailheads))
