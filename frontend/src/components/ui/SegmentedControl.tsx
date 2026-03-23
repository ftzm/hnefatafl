import { For } from "solid-js";
import type { SelectOption } from "./types";

interface SegmentedControlProps {
  options: SelectOption[];
  value: string;
  onChange: (value: string) => void;
}

export default function SegmentedControl(props: SegmentedControlProps) {
  return (
    <div class="segmented-control" role="radiogroup">
      <For each={props.options}>
        {(option) => (
          <button
            type="button"
            role="radio"
            aria-checked={props.value === option.value}
            class={`segmented-option${props.value === option.value ? " selected" : ""}`}
            onClick={() => props.onChange(option.value)}
          >
            {option.label}
          </button>
        )}
      </For>
    </div>
  );
}
