import type { JSX } from "solid-js";

export default function SkipForwardIcon(props: JSX.SvgSVGAttributes<SVGSVGElement>) {
  return (
    <svg viewBox="0 0 16 16" {...props}>
      <polygon points="2,3 10,8 2,13" />
      <rect x="12" y="3" width="2" height="10" />
    </svg>
  );
}
