import { For, Show } from "solid-js";
import Clock from "./Clock.jsx";

export default function PlayerBar(props) {
  const isBlack = () => props.color === "black";
  const capClass = () => (isBlack() ? "cap-white" : "cap-black");

  return (
    <div
      class={`player-bar ${props.color}-player${props.active ? " active" : ""}`}
    >
      <span class="name">{props.name || (isBlack() ? "Black" : "White")}</span>
      <span class="captures">
        <For each={Array.from({ length: props.capturedCount })}>
          {() => <span class={`cap-piece ${capClass()}`} />}
        </For>
      </span>
      <Show when={props.timeControl} fallback={<span class="clock">--:--</span>}>
        <Clock
          initialSeconds={props.timeControl?.initial}
          active={props.active}
        />
      </Show>
    </div>
  );
}
