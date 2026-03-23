import { useParams } from "@solidjs/router";
import { createEffect, on, onCleanup, onMount } from "solid-js";
import { useOnlineGame } from "../api/online-game-context";
import type { Move } from "../board-logic";
import GameLayout from "../components/GameLayout";
import { GameProvider, useGame } from "../game-context";

function OnlineController() {
  const game = useGame();
  const online = useOnlineGame();
  const params = useParams<{ id: string }>();

  onMount(() => {
    online.connect(params.id);
  });

  onCleanup(() => {
    online.disconnect();
  });

  createEffect(
    on(online.events, (event) => {
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
                ? { black: "You", white: "Opponent" }
                : { black: "Opponent", white: "You" },
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
        case "undoAccepted":
          for (let i = 0; i < event.moveCount; i++) {
            game.undoLastMove();
          }
          break;
        case "opponentJoined":
        case "opponentLeft":
        case "drawOffer":
        case "drawDeclined":
        case "undoRequest":
        case "undoDeclined":
        case "chat":
          break;
      }
    }),
  );

  function onMove(move: Move) {
    game.applyMove(move);
    online.sendMove(move);
  }

  function onResign() {
    online.resign();
  }

  function onDraw() {
    online.offerDraw();
  }

  function onUndo() {
    online.requestUndo();
  }

  return (
    <GameLayout
      mode="online"
      onMove={onMove}
      onResign={onResign}
      onDraw={onDraw}
      onUndo={onUndo}
    />
  );
}

export default function OnlineGame() {
  return (
    <GameProvider>
      <OnlineController />
    </GameProvider>
  );
}
