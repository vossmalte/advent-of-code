import fs from "fs";

const f = fs.readFileSync("input-real.txt", { encoding: "utf8" });

const onlyDigits = f
  .split("\n")
  .filter(Boolean)
  .map((line) => [...line].filter((char) => /\d/.test(char)).join(""));

console.log(onlyDigits);

const onlyRelevantDigits = onlyDigits.map(
  (line) => `${line.at(0)}${line.at(-1)}`
);

console.log(onlyRelevantDigits);

const x = onlyRelevantDigits.map(Number).reduce((p, c) => p + c, 0);

console.log(x);
