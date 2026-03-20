import { splitProps, type ParentProps, type JSX } from "solid-js";

type ButtonGroupProps = ParentProps<JSX.HTMLAttributes<HTMLDivElement>>;

export default function ButtonGroup(props: ButtonGroupProps) {
  const [local, rest] = splitProps(props, ["class", "children"]);

  return (
    <div
      role="group"
      class={`btn-group${local.class ? ` ${local.class}` : ""}`}
      {...rest}
    >
      {local.children}
    </div>
  );
}
