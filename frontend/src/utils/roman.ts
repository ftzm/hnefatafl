// Convert a positive integer to a lowercase Roman numeral.
// Used for the marginalia-style move numbers in MoveHistory.
const ROMAN_PAIRS: ReadonlyArray<readonly [number, string]> = [
  [1000, "m"],
  [900, "cm"],
  [500, "d"],
  [400, "cd"],
  [100, "c"],
  [90, "xc"],
  [50, "l"],
  [40, "xl"],
  [10, "x"],
  [9, "ix"],
  [5, "v"],
  [4, "iv"],
  [1, "i"],
];

export function toRomanLower(n: number): string {
  if (!Number.isFinite(n) || n <= 0) return "";
  let remaining = Math.floor(n);
  let out = "";
  for (const [value, numeral] of ROMAN_PAIRS) {
    while (remaining >= value) {
      out += numeral;
      remaining -= value;
    }
  }
  return out;
}
