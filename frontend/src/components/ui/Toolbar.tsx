import { type JSX, type ParentProps, splitProps } from "solid-js";

type ToolbarProps = ParentProps<JSX.HTMLAttributes<HTMLDivElement>>;

export default function Toolbar(props: ToolbarProps) {
  const [local, rest] = splitProps(props, ["class", "children"]);

  return (
    <div
      role="toolbar"
      class={`toolbar${local.class ? ` ${local.class}` : ""}`}
      {...rest}
    >
      {local.children}
    </div>
  );
}
