import numpy as np
from importlib import reload

# fileName = "input-test.txt"
fileName = "input.txt"

input = np.loadtxt(
    fileName,
    dtype=np.dtype("<U1"),
    delimiter=" ",
)

(maxX, maxY) = input.shape
maxSize = max(maxX, maxY)

frequencies = np.unique(input)


# calculate antinodes
def calculate_antinodes(frequency):
    if frequency == ".":
        return []
    result = []
    antennas = np.argwhere(input == frequency)
    for i in antennas:
        for j in antennas:
            if np.all(i == j):
                break
            diff = i - j
            result.append(i + diff)
            result.append(j - diff)
    return result


antinodes = []
for f in frequencies:
    antinodes.extend(calculate_antinodes(f))


def inBounds(antinode):
    [x, y] = antinode
    return maxX > x and maxY > y and 0 <= x and 0 <= y


def filterAntinodes(antinodes):
    return np.unique(list(filter(inBounds, list(antinodes))), axis=0)


result1 = len(list(filterAntinodes(antinodes)))
print("Part 1:", result1)


# count


# calculate antinodes
def calculate_antinodes2(frequency):
    if frequency == ".":
        return []
    antennas = np.argwhere(input == frequency)
    if len(antennas) == 1:
        return []
    result = []
    for i in antennas:
        for j in antennas:
            if np.all(i == j):
                break
            diff = i - j
            line = [i + k * diff for k in range(-maxSize, maxSize)]
            result.extend(line)
    return result


antinodes2 = []
for f in frequencies:
    antinodes2.extend(calculate_antinodes2(f))
    
result2 = len(list(filterAntinodes(antinodes2)))
print("Part 2:", result2)
