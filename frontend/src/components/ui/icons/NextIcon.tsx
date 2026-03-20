import type { JSX } from "solid-js";

export default function NextIcon(props: JSX.SvgSVGAttributes<SVGSVGElement>) {
  return (
    <svg viewBox="0 0 16 16" {...props}>
      <polygon points="4,3 12,8 4,13" />
    </svg>
  );
}
