import { type JSX, type ParentProps, splitProps } from "solid-js";

type ButtonGroupProps = ParentProps<
  JSX.FieldsetHTMLAttributes<HTMLFieldSetElement>
>;

export default function ButtonGroup(props: ButtonGroupProps) {
  const [local, rest] = splitProps(props, ["class", "children"]);

  return (
    <fieldset
      class={`btn-group${local.class ? ` ${local.class}` : ""}`}
      {...rest}
    >
      {local.children}
    </fieldset>
  );
}
