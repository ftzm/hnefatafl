import { Button as KobalteButton } from "@kobalte/core/button";
import { type JSX, mergeProps, type ParentProps, splitProps } from "solid-js";

interface ButtonProps extends ParentProps {
  variant?: "solid" | "ghost";
  class?: string;
  disabled?: boolean;
  type?: "button" | "submit" | "reset";
  "aria-label"?: string;
  onClick?: JSX.EventHandlerUnion<HTMLButtonElement, MouseEvent>;
}

export default function Button(props: ButtonProps) {
  const merged = mergeProps({ variant: "solid" as const }, props);
  const [local, rest] = splitProps(merged, ["variant", "class", "children"]);

  return (
    <KobalteButton
      class={`btn btn-${local.variant}${local.class ? ` ${local.class}` : ""}`}
      {...rest}
    >
      {local.children}
    </KobalteButton>
  );
}
