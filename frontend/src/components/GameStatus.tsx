import { useGame } from "../game-context";

export default function GameStatus() {
  const game = useGame();

  const statusText = () => {
    if (game.store.game.gameOver) {
      if (game.store.game.gameOver.winner === "draw") return "Draw";
      const winner =
        game.store.game.gameOver.winner === "white" ? "White" : "Black";
      return `${winner} wins — ${game.store.game.gameOver.reason}`;
    }
    const player =
      game.store.game.currentPlayer === "black" ? "Black" : "White";
    return `${player} to move`;
  };

  return <div class="game-status">{statusText()}</div>;
}
