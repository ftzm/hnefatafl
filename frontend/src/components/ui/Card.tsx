import { splitProps, type ParentProps, type JSX } from "solid-js";

type CardProps = ParentProps<JSX.HTMLAttributes<HTMLDivElement>>;

export default function Card(props: CardProps) {
  const [local, rest] = splitProps(props, ["class", "children"]);
  return (
    <div class={`card${local.class ? ` ${local.class}` : ""}`} {...rest}>
      {local.children}
    </div>
  );
}
