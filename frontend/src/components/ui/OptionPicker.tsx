import { createEffect, createUniqueId, For, onMount } from "solid-js";
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
  // Unique radio-group name so multiple pickers in the same modal don't
  // share a group (which would let arrow keys jump between unrelated fields).
  const groupName = `option-picker-${createUniqueId()}`;

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
    <div class="option-picker" ref={containerRef}>
      <For each={props.options}>
        {(option) => {
          const selected = () => props.value === option.value;
          return (
            <label class={`option${selected() ? " selected" : ""}`}>
              <input
                type="radio"
                name={groupName}
                value={option.value}
                checked={selected()}
                onChange={() => props.onChange(option.value)}
              />
              <span
                class="option-label"
                ref={(el) => labelRefs.set(option.value, el)}
              >
                {option.label}
              </span>
            </label>
          );
        }}
      </For>
      <span class="option-picker-bar" ref={barRef} />
    </div>
  );
}
