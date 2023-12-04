import fs from "fs";

const reset = "\x1b[0m";
const highlightFG = "\x1b[30m";
const highlightBG = "\x1b[43m";

function highlight(input: string) {
  return highlightBG + highlightFG + input + reset;
}

function main(lines: String[]): number {
  const parsed = lines.map((line) => {
    const split = line.replace(/ +/g, " ").split(/.*: | \| /);
    return [split[1] ?? "x", split[2] ?? "y"] as const;
  });

  const lists = parsed.map((numbers) => numbers.map((n) => n.split(/ +/)));

  const countWinners = lists.map(([winningNumbers, myNumbers]) =>
    myNumbers.reduce((p, c) => p + (winningNumbers.includes(c) ? 1 : 0), 0),
  );

  const _highlightWinners = lists.map(([w, ns]) =>
    ns.map((n) => (w.includes(n) ? highlight(n) : n)),
  );

  const powers = countWinners.map((c) => c && Math.pow(2, c - 1));

  const result = powers.reduce((p, c) => p + c);

  // // pretty print :)
  // lists.forEach(([winningNumbers], index) =>
  //   console.log(
  //     index,
  //     ":",
  //     winningNumbers.join(" "),
  //     "|",
  //     _highlightWinners[index].join(" "),
  //     "->",
  //     countWinners[index],
  //     powers[index],
  //   ),
  // );
  return result;
}

type ID = number & { id: never };

function main2(lines: string[]): number {
  const parsed = lines.filter(Boolean).map((line) => {
    const split = line.replace(/ +/g, " ").split(/.*: | \| /);
    return [split[1] ?? "x", split[2] ?? "y"] as const;
  });

  const lists = parsed.map((numbers) => numbers.map((n) => n.split(/ +/)));

  const countWinners = lists.map(([winningNumbers, myNumbers]) =>
    myNumbers.reduce((p, c) => p + (winningNumbers.includes(c) ? 1 : 0), 0),
  );

  const countMap = new Map(countWinners.map((_, index) => [index as ID, 1]));

  countMap.forEach((v, k, map) => {
    for (let i = k + 1; i <= k + countWinners[k]; i++) {
      map.set(i as ID, (map.get(i as ID) ?? 0) + v);
    }
  });

  return [...countMap.values()].reduce((a, b) => a + b);
}

const lines1 = fs
  .readFileSync("input-first.txt", { encoding: "utf8" })
  .split("\n");
const result1 = main(lines1);
if (result1 !== 13) console.warn("test 1 failed");
const result2 = main2(lines1);
if (result2 !== 30) console.warn("test 2 failed");

const lines = fs.readFileSync("input.txt", { encoding: "utf8" }).split("\n");
console.log(highlight("puzzle 1:"), main(lines));
console.log("puzzle 2: ", main2(lines));
