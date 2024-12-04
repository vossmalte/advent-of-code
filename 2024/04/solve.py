import numpy as np

fileName = "input.txt"
# fileName = "input-test.txt"

# format:
# %s/\(\w\)/\1 /g
# %s/ $//g

arr = np.loadtxt(
    fileName,
    dtype=np.dtype("<U1"),
    delimiter=" ",
)

maxDim = max(arr.shape)

lines = []

lines.extend(list(arr))

lines.extend(list(arr.T))

lines.extend([arr.diagonal(i) for i in range(-maxDim, maxDim)])

lines.extend([np.fliplr(arr).diagonal(i) for i in range(-maxDim, maxDim)])

word = ";".join(["".join(line) for line in lines])

result = word.count("XMAS") + word.count("SAMX")

print(result)
