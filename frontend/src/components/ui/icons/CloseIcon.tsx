import type { JSX } from "solid-js";

export default function CloseIcon(props: JSX.SvgSVGAttributes<SVGSVGElement>) {
  return (
    <svg aria-hidden="true" viewBox="0 0 16 16" {...props}>
      <path
        d="M4 4l8 8M12 4l-8 8"
        stroke="currentColor"
        stroke-width="1.5"
        fill="none"
      />
    </svg>
  );
}
