import { Button as KobalteButton } from "@kobalte/core/button";
import { splitProps, mergeProps } from "solid-js";

export default function Button(props) {
  const merged = mergeProps({ variant: "solid" }, props);
  const [local, rest] = splitProps(merged, [
    "variant",
    "class",
    "children",
  ]);

  return (
    <KobalteButton
      class={`btn btn-${local.variant}${local.class ? ` ${local.class}` : ""}`}
      {...rest}
    >
      {local.children}
    </KobalteButton>
  );
}
