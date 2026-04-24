import { For, createEffect, onMount } from "solid-js";
import type { SelectOption } from "./types";

interface ChipGridProps {
  options: SelectOption[];
  value: string;
  onChange: (value: string) => void;
}

export default function ChipGrid(props: ChipGridProps) {
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
    <div class="chip-grid" role="radiogroup" ref={containerRef}>
      <For each={props.options}>
        {(option) => (
          <button
            type="button"
            role="radio"
            aria-checked={props.value === option.value}
            class={`chip${props.value === option.value ? " selected" : ""}`}
            onClick={() => props.onChange(option.value)}
          >
            <span class="chip-label" ref={(el) => labelRefs.set(option.value, el)}>
              {option.label}
            </span>
          </button>
        )}
      </For>
      <span class="chip-bar" ref={barRef} />
    </div>
  );
}
