export interface SelectOption {
  value: string;
  label: string;
}

export const sideOptions: SelectOption[] = [
  { value: "black", label: "Black" },
  { value: "random", label: "Random" },
  { value: "white", label: "White" },
];

export const timeOptions: SelectOption[] = [
  { value: "none", label: "None" },
  { value: "5+0", label: "5 min" },
  { value: "5+3", label: "5 | 3" },
  { value: "10+0", label: "10 min" },
  { value: "10+5", label: "10 | 5" },
  { value: "15+10", label: "15 | 10" },
];
