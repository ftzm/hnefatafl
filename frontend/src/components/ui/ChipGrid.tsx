import { For } from "solid-js";
import type { SelectOption } from "../../gameOptions";

interface ChipGridProps {
  options: SelectOption[];
  value: string;
  onChange: (value: string) => void;
}

export default function ChipGrid(props: ChipGridProps) {
  return (
    <div class="chip-grid" role="radiogroup">
      <For each={props.options}>
        {(option) => (
          <button
            role="radio"
            aria-checked={props.value === option.value}
            class={`chip${props.value === option.value ? " selected" : ""}`}
            onClick={() => props.onChange(option.value)}
          >
            {option.label}
          </button>
        )}
      </For>
    </div>
  );
}
