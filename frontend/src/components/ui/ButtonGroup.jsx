import { splitProps } from "solid-js";

export default function ButtonGroup(props) {
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
