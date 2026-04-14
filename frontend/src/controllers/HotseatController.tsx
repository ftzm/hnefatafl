import { useParams } from "@solidjs/router";
import { useHotseatApi } from "../api/contexts";
import type { Move } from "../board-logic";
import GameLayout from "../components/GameLayout";
import { GameProvider, useGame } from "../game-context";

function HotseatController() {
  const game = useGame();
  const hotseat = useHotseatApi();
  const params = useParams<{ id: string }>();

  hotseat.getState(params.id).then((state) =>
    game.initGame({
      boardRep: state.boardRep,
      currentPlayer: state.currentPlayer,
      moves: state.moves,
      moveHistory: state.moveHistory,
      gameOver: state.gameOver,
    }),
  );

  function onMove(move: Move) {
    game.applyMove(move);
    hotseat.sendMove(params.id, move).then((r) => game.reconcile(r));
  }

  function onUndo() {
    game.undoLastMove();
    hotseat.undo(params.id).then((r) => game.reconcile(r));
  }

  function onResign() {
    hotseat
      .resign(params.id, game.store.game.currentPlayer)
      .then((r) => game.reconcile(r));
  }

  function onDraw() {
    hotseat.draw(params.id).then((r) => game.reconcile(r));
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
