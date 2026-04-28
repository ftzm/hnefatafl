import { createEffect, For, onMount } from "solid-js";
import type { SelectOption } from "./types";

interface OptionPickerProps {
  options: SelectOption[];
  value: string;
  onChange: (value: string) => void;
}

export default function OptionPicker(props: OptionPickerProps) {
  let containerRef!: HTMLDivElement;
  let barRef!: HTMLSpanElement;
  const labelRefs = new Map<string, HTMLSpanElement>();

  const updateBar = () => {
    const el = labelRefs.get(props.value);
    if (!el || !containerRef) return;
    const containerRect = containerRef.getBoundingClientRect();
    const labelRect = el.getBoundingClientRect();
    barRef.style.left = `${labelRect.left - containerRect.left}px`;
    barRef.style.width = `${labelRect.width}px`;
  };

  onMount(updateBar);
  createEffect(updateBar);

  return (
    <div class="option-picker" role="radiogroup" ref={containerRef}>
      <For each={props.options}>
        {(option) => (
          <button
            type="button"
            role="radio"
            aria-checked={props.value === option.value}
            class={`option${props.value === option.value ? " selected" : ""}`}
            onClick={() => props.onChange(option.value)}
          >
            <span
              class="option-label"
              ref={(el) => labelRefs.set(option.value, el)}
            >
              {option.label}
            </span>
          </button>
        )}
      </For>
      <span class="option-picker-bar" ref={barRef} />
    </div>
  );
}
