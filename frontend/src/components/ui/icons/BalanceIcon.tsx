import type { JSX } from "solid-js";

export default function BalanceIcon(props: JSX.SvgSVGAttributes<SVGSVGElement>) {
  return (
    <svg viewBox="0 0 24 24" {...props}>
      {/* Center post */}
      <line x1="12" y1="3" x2="12" y2="21" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
      {/* Beam */}
      <line x1="4" y1="7" x2="20" y2="7" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
      {/* Left pan */}
      <path d="M4 7l-1 7h6l-1-7" stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round" />
      {/* Right pan */}
      <path d="M20 7l1 7h-6l1-7" stroke="currentColor" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round" />
      {/* Base */}
      <line x1="8" y1="21" x2="16" y2="21" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
    </svg>
  );
}
