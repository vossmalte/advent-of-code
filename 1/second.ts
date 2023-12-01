import fs from "fs";

const f = fs.readFileSync("input-real.txt", { encoding: "utf8" });
const lines = f.split("\n").filter(Boolean);

// have a smart order
// e.g. "twoone" is just "2ne"
// one eight two is kind of a bummer as this is a cycle
// even smaller: eight three
// eight is the bad guy so we pad it to be eeightt
//
// we want to replace those words first that end with something another word starts with
// no, we want it the other way round:
// first replace the words that may get truncated
// seveNINE twONE
const translations = [
  ["eight", "e8t"],
  ["nine", "9e"],
  ["one", "o1e"],
  ["four", "4"],
  ["five", "5e"],
  ["six", "6"],
  ["seven", "7n"],
  ["three", "t3e"],
  ["two", "t2o"],
] as const;

const replacedFirstWords = translations.reduce(
  (lines, [word, digit]) => lines.map((line) => line.replace(word, digit)),
  lines
);

function reverse(str: string) {
  return [...str].reverse().join("");
}

// i thought there was a replaceAll method, but there is not
// so to get the last word, we reverse and replace with reversed words...

const replacedLastWords = translations
  .reduce(
    (lines, [word, digit]) =>
      lines.map((line) => line.replace(reverse(word), digit)),
    replacedFirstWords.map(reverse)
  )
  .map(reverse);

console.log(replacedLastWords);

const onlyDigits = replacedLastWords.map((line) =>
  [...line].filter((char) => /\d/.test(char)).join("")
);

// console.log(onlyDigits);

const onlyRelevantDigits = onlyDigits.map(
  (line) => `${line.at(0)}${line.at(-1)}`
);

// console.log(onlyRelevantDigits);

const x = onlyRelevantDigits.map(Number).reduce((p, c) => p + c, 0);

console.log(x);
