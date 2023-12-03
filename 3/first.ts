import fs from "fs";

// match all that is not word or dot
const specialCharRegex = /[^\w\.]/g;

// match all stars
const starRegex = /\*/g;

// match all numbers
const numberRegex = /\d+/g;

// is number next to special char?
function neighbors(
  [numberX, numberY]: readonly [number, number],
  numberLength: number,
  [specialCharX, specialCharY]: readonly [number, number]
): boolean {
  return (
    Math.abs(specialCharY - numberY) < 2 &&
    specialCharX + 1 >= numberX &&
    specialCharX - 1 <= numberX + numberLength - 1
  );
}

function main(lines: String[]): number {
  const numberMatches = lines.flatMap((line, index) =>
    [...line.matchAll(numberRegex)].map(
      (match) =>
        [[match.index!, index], match[0].length, parseInt(match[0])] as const
    )
  );
  const specialMatchesPositions = lines.flatMap((line, index) =>
    [...line.matchAll(specialCharRegex)].map(
      (match) => [match.index!, index] as const
    )
  );

  const onlyNumbersWithNeighbors = numberMatches
    .filter(([position, len]) =>
      specialMatchesPositions.some((specialPosition) =>
        neighbors(position, len, specialPosition)
      )
    )
    .map(([_position, _len, value]) => value);

  const result = onlyNumbersWithNeighbors.reduce((a, b) => a + b);

  // lines.forEach((line, index) => console.log(line, numberMatches[index]));
  return result;
}

function main2(lines: String[]): number {
  const numberMatches = lines.flatMap((line, index) =>
    [...line.matchAll(numberRegex)].map(
      (match) =>
        [[match.index!, index], match[0].length, parseInt(match[0])] as const
    )
  );
  const starPositions = lines.flatMap((line, index) =>
    [...line.matchAll(starRegex)].map((match) => [match.index!, index] as const)
  );

  // not necessary, just filter by results.length === 2
  // const gearPositions = starPositions.filter(
  //   (starPosition) =>
  //     numberMatches.findIndex(([numberPosition, numberLength]) =>
  //       neighbors(numberPosition, numberLength, starPosition)
  //     ) !==
  //     numberMatches.length -
  //       1 -
  //       numberMatches
  //         .slice()
  //         .reverse()
  //         .findIndex(([numberPosition, numberLength]) =>
  //           neighbors(numberPosition, numberLength, starPosition)
  //         )
  // );

  const gearRatios = starPositions
    .map((gearPosition) =>
      numberMatches.filter(([numberPosition, numberLength]) =>
        neighbors(numberPosition, numberLength, gearPosition)
      )
    )
    .filter((numbers) => numbers.length === 2)
    .map(([a, b]) => a[2] * b[2]);

  const result = gearRatios.reduce((a, b) => a + b);

  return result;
}

const lines1 = fs
  .readFileSync("input-first.txt", { encoding: "utf8" })
  .split("\n");
const result1 = main(lines1);
if (result1 !== 4361) console.warn("test 1 failed");
const result2 = main2(lines1);
if (result2 !== 467835) console.warn("test 2 failed");

const lines = fs.readFileSync("input.txt", { encoding: "utf8" }).split("\n");
console.log("puzzle 1: ", main(lines));
console.log("puzzle 2: ", main2(lines));
