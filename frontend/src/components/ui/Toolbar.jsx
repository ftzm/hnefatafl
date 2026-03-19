import { splitProps } from "solid-js";

export default function Toolbar(props) {
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
