import numpy as np
from scipy.ndimage import convolve

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


def mapArr(op, a):
    return np.array(list(map(op, a.flat))).reshape(a.shape)


# convert to numbers
arrn = mapArr(ord, arr)


diagonal = np.dot(np.array(list(map(ord, "MAS"))), np.array([2, 13, 2]))
counter_diagonal = np.dot(np.array(list(map(ord, "MAS"))), np.array([3, 0, 3]))
interesting_result = diagonal + counter_diagonal

# kernel consists of prime numbers to generate unique result
kernel = np.array([[2, 0, 3], [0, 13, 0], [3, 0, 2]])

c = convolve(
    arrn,
    kernel,
    mode="constant",
)

result = np.sum(c == interesting_result)
print(result)
