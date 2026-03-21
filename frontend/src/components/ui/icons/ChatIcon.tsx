import type { JSX } from "solid-js";

export default function ChatIcon(props: JSX.SvgSVGAttributes<SVGSVGElement>) {
  return (
    <svg viewBox="0 0 16 16" {...props}>
      <path d="M2 3h12v8H5l-3 3V3z" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linejoin="round" />
    </svg>
  );
}
