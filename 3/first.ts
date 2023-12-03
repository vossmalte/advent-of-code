import fs from "fs";

// match all that is not word or dot
const specialCharRegex = /[^\w\.]/g;

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

  const result = onlyNumbersWithNeighbors.reduce((a,b)=>a+b)

  // lines.forEach((line, index) => console.log(line, numberMatches[index]));
  return result;
}

const lines1 = fs
  .readFileSync("input-first.txt", { encoding: "utf8" })
  .split("\n");
const result1 = main(lines1);
if (result1 !== 4361) console.warn("test 1 failed");

const lines = fs
  .readFileSync("input.txt", { encoding: "utf8" })
  .split("\n");
console.log(main(lines));
