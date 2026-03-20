import type { JSX } from "solid-js";

export default function PrevIcon(props: JSX.SvgSVGAttributes<SVGSVGElement>) {
  return (
    <svg viewBox="0 0 16 16" {...props}>
      <polygon points="12,3 4,8 12,13" />
    </svg>
  );
}
