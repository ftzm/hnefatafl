import { createSignal, createEffect, onCleanup } from "solid-js";

export default function Clock(props) {
  const [remaining, setRemaining] = createSignal(props.initialSeconds);

  createEffect(() => {
    if (!props.active) return;
    const interval = setInterval(() => {
      setRemaining((prev) => {
        if (prev <= 0) {
          clearInterval(interval);
          return 0;
        }
        return prev - 1;
      });
    }, 1000);
    onCleanup(() => clearInterval(interval));
  });

  const formatTime = (secs) => {
    const m = Math.floor(secs / 60);
    const s = secs % 60;
    return `${m}:${s.toString().padStart(2, "0")}`;
  };

  return <span class="clock">{formatTime(remaining())}</span>;
}
