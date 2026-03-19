import { For } from "solid-js";

export default function PlayerBar(props) {
  const isBlack = () => props.color === "black";
  // Black player captures white pieces, white captures black
  const capClass = () => (isBlack() ? "cap-white" : "cap-black");

  return (
    <div
      class={`player-bar ${props.color}-player${props.active ? " active" : ""}`}
    >
      <span class="name">{isBlack() ? "Black" : "White"}</span>
      <span class="captures">
        <For each={Array.from({ length: props.capturedCount })}>
          {() => <span class={`cap-piece ${capClass()}`} />}
        </For>
      </span>
      <span class="clock">--:--</span>
    </div>
  );
}
