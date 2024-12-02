left = []
right = []
with open("input.txt", "r") as f:
    lines = f.readlines()
    for line in lines:
        [l, r] = line.split("   ")
        left.append(int(l))
        right.append(int(r))

zipped = zip(sorted(left), sorted(right))

delta = [abs((l) - (r)) for l, r in zipped]

result = sum(delta)

print(result)

counts = [(int(l), right.count(l)) for l in left]

weights = [l * c for l, c in counts]

result2 = sum(weights)

print(result2)
