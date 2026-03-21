import { Show } from "solid-js";
import { store } from "../state";

export default function GameStatus() {
  const gameActive = () => !store.game.gameOver;

  const statusText = () => {
    if (store.game.gameOver) {
      if (store.game.gameOver.winner === "draw") return "Draw";
      const winner = store.game.gameOver.winner === "white" ? "White" : "Black";
      return `${winner} wins — ${store.game.gameOver.reason}`;
    }
    const player = store.game.currentPlayer === "black" ? "Black" : "White";
    return `${player} to move`;
  };

  return (
    <div class="game-status">
      {statusText()}
      <Show when={gameActive()}>
        <span class="thinking-dots">
          <span>.</span>
          <span>.</span>
          <span>.</span>
        </span>
      </Show>
    </div>
  );
}
