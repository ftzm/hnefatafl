import type { SelectOption } from "./components/ui/types";

export const sideOptions: SelectOption[] = [
  { value: "black", label: "Attackers" },
  { value: "white", label: "Defenders" },
  { value: "random", label: "Random" },
];

export const difficultyOptions: SelectOption[] = [
  { value: "easy", label: "Easy" },
  { value: "medium", label: "Medium" },
  { value: "hard", label: "Hard" },
];

export const timeOptions: SelectOption[] = [
  { value: "none", label: "Untimed" },
  { value: "5+0", label: "5 min" },
  { value: "10+0", label: "10 min" },
  { value: "15+0", label: "15 min" },
];
