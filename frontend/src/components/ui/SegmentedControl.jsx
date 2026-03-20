import { For } from "solid-js";

export default function SegmentedControl(props) {
  return (
    <div class="segmented-control" role="radiogroup">
      <For each={props.options}>
        {(option) => (
          <button
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
