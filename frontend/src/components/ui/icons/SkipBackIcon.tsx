import type { JSX } from "solid-js";

export default function SkipBackIcon(
  props: JSX.SvgSVGAttributes<SVGSVGElement>,
) {
  return (
    <svg aria-hidden="true" viewBox="0 0 16 16" {...props}>
      <rect x="2" y="3" width="2" height="10" />
      <polygon points="14,3 6,8 14,13" />
    </svg>
  );
}
