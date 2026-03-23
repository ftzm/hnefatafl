import { useHotseatApi } from "../api/hotseat-context";
import type { Move } from "../board-logic";
import GameLayout from "../components/GameLayout";
import { GameProvider, useGame } from "../game-context";

function HotseatController() {
  const game = useGame();
  const hotseat = useHotseatApi();

  hotseat.getGameState([]).then((r) => game.reconcile(r));

  function onMove(move: Move) {
    game.applyMove(move);
    hotseat
      .getGameState(game.store.game.moveHistory)
      .then((r) => game.reconcile(r));
  }

  function onUndo() {
    game.undoLastMove();
    hotseat
      .getGameState(game.store.game.moveHistory)
      .then((r) => game.reconcile(r));
  }

  function onResign() {
    const winner =
      game.store.game.currentPlayer === "black" ? "white" : "black";
    game.setGameOver({ winner, reason: "Resignation" });
  }

  function onDraw() {
    game.setGameOver({ winner: "draw", reason: "Draw agreed" });
  }

  return (
    <GameLayout
      mode="hotseat"
      onMove={onMove}
      onUndo={onUndo}
      onResign={onResign}
      onDraw={onDraw}
    />
  );
}

export default function HotseatGame() {
  return (
    <GameProvider>
      <HotseatController />
    </GameProvider>
  );
}
