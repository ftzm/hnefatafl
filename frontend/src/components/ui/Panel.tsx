import type { ParentProps } from "solid-js";

interface PanelProps extends ParentProps {
  class?: string;
}

export default function Panel(props: PanelProps) {
  return (
    <div class={`panel${props.class ? ` ${props.class}` : ""}`}>
      {props.children}
    </div>
  );
}
