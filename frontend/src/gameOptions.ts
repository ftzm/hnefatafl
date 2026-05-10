import type { SelectOption } from "./components/ui/types";

export const sideOptions: SelectOption[] = [
  { value: "black", label: "Attackers" },
  { value: "white", label: "Defenders" },
  { value: "random", label: "Random" },
];

export interface TimeControlValue {
  initialTime: number;
  increment: number;
}

export interface TimeOption extends SelectOption {
  timeControl: TimeControlValue | null;
}

function formatTimeControl(tc: TimeControlValue): string {
  const mins = Math.floor(tc.initialTime / 60);
  if (tc.increment === 0) return `${mins} min`;
  return `${mins}+${tc.increment}`;
}

function createTimeOption(tc: TimeControlValue): TimeOption {
  const label = formatTimeControl(tc);
  return { value: label, label, timeControl: tc };
}

export const timeOptions: TimeOption[] = [
  { value: "none", label: "Untimed", timeControl: null },
  createTimeOption({ initialTime: 300, increment: 0 }),
  createTimeOption({ initialTime: 600, increment: 0 }),
  createTimeOption({ initialTime: 900, increment: 0 }),
];

export function getTimeControl(value: string): TimeControlValue | null {
  const opt = timeOptions.find((o) => o.value === value);
  if (!opt) throw new Error(`Unknown time control value: ${value}`);
  return opt.timeControl;
}
