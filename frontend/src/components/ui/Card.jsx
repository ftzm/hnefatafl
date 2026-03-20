import { splitProps } from "solid-js";

export default function Card(props) {
  const [local, rest] = splitProps(props, ["class", "children"]);
  return (
    <div class={`card${local.class ? ` ${local.class}` : ""}`} {...rest}>
      {local.children}
    </div>
  );
}
