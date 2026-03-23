import { useParams } from "@solidjs/router";
import { createEffect, on, onCleanup, onMount } from "solid-js";
import { useAiGame } from "../api/contexts";
import type { Move } from "../board-logic";
import GameLayout from "../components/GameLayout";
import { GameProvider, useGame } from "../game-context";

function AiController() {
  const game = useGame();
  const ai = useAiGame();
  const params = useParams<{ id: string }>();

  onMount(() => {
    ai.connect(params.id);
  });

  onCleanup(() => {
    ai.disconnect();
  });

  createEffect(
    on(ai.events, (event) => {
      if (!event) return;

      switch (event.type) {
        case "initialState":
          game.initGame({
            boardRep: event.boardRep,
            currentPlayer: event.currentPlayer,
            moves: event.moves,
            moveHistory: event.moveHistory,
            playerColor: event.playerColor,
            gameOver: event.gameOver,
            players:
              event.playerColor === "black"
                ? { black: "You", white: "AI" }
                : { black: "AI", white: "You" },
          });
          break;
        case "moveMade":
          if (event.currentPlayer === game.store.game.playerColor) {
            game.applyExternalMove(event);
          }
          break;
        case "gameOver":
          game.setGameOver({ winner: event.winner, reason: event.reason });
          break;
      }
    }),
  );

  function onMove(move: Move) {
    game.applyMove(move);
    ai.sendMove(move);
  }

  function onResign() {
    ai.resign();
  }

  function onUndo() {
    game.undoLastMove();
    game.undoLastMove();
  }

  return (
    <GameLayout mode="ai" onMove={onMove} onResign={onResign} onUndo={onUndo} />
  );
}

export default function AiGame() {
  return (
    <GameProvider>
      <AiController />
    </GameProvider>
  );
}
