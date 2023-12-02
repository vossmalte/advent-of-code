
def parseLine(line):
    [gameName, game] = line.split(": ")
    gameId = int(gameName.split(" ")[1])

    return (gameId, game)

    pass

def parseGame(game):
    draws = [ parseDraw(draw) for draw in game.split("; ") ]
    return draws

def parseDraw(draw):
    "draw: '1 green, 1 blue'"
    numberOfCubes = {
        "red": 0,
        "green": 0,
        "blue": 0,
    }
    # print("draw",draw)
    for amount_color in draw.split(', '):
        # print(amount_color)
        [amount,color] = amount_color.split(' ')
        numberOfCubes[color]=int(amount)
    return numberOfCubes

maxNumberOfCubes = {
    "red": 12,
    "green": 13,
    "blue": 14,
}

def isDrawIllegal(draw):
    "game is a dictionary"
    # print(draw)
    for color in list(maxNumberOfCubes):
        if draw[color] > maxNumberOfCubes[color]:
            return True
    return False
    



def main(lines):
    # no empty lines
    lines = [line.replace("\n","") for line in lines if line != "\n"]

    result = 0
    for line in lines:
        (id, game) = parseLine(line)
        draws = parseGame(game)
        isIllegal = any(map(isDrawIllegal,draws))
        # print(id, isIllegal)
        if not isIllegal:
            result += id
    return result

        
def powerOfSet(set):
    result=1
    for color in list(set):
        result*=set[color]
    return result

def main2(lines):
    # no empty lines
    lines = [line.replace("\n","") for line in lines if line != "\n"]

    result = 0
    for line in lines:
        (id, game) = parseLine(line)
        draws = parseGame(game)
        min_dict = {}
        for color in list(maxNumberOfCubes):
            min_dict[color] = max([draw[color] for draw in draws])
        # print(min_dict)
        result += powerOfSet(min_dict)
    return result


if __name__ == "__main__":
    if True:
        # print(parseDraw("3 green, 3 blue"))
        pass
    # test
    with open("input-first.txt") as file:
        result = main(file.readlines())
        if not result == 8:
            print("first test failed")
    with open("input-second.txt") as file:
        result = main2(file.readlines())
        if not result == 2286:
            print("second test failed")
    
    with open("input.txt") as file:
        # result1 = main(file.readlines())
        result2 = main2(file.readlines())
    
        print(result2)
        pass
