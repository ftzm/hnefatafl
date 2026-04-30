import { useParams } from "@solidjs/router";
import { createEffect, createSignal, on, onCleanup, onMount } from "solid-js";
import { useOnlineGame } from "../api/contexts";
import type { Move, PlayerColor } from "../board-logic";
import GameLayout from "../components/GameLayout";
import OfferBanner, {
  type BannerNotice,
  type IncomingOffer,
} from "../components/OfferBanner";
import { GameProvider, useGame } from "../game-context";

const NOTICE_AUTO_DISMISS_MS = 4000;

function opponentName(
  by: PlayerColor,
  players: { black: string; white: string } | undefined,
  selfColor: PlayerColor | null,
): string {
  if (selfColor && by === selfColor) return "You";
  if (!players) return "Opponent";
  return by === "black" ? players.black : players.white;
}

function OnlineController() {
  const game = useGame();
  const online = useOnlineGame();
  const params = useParams<{ id: string }>();

  const [incoming, setIncoming] = createSignal<IncomingOffer | undefined>();
  const [outgoingDraw, setOutgoingDraw] = createSignal(false);
  const [outgoingUndo, setOutgoingUndo] = createSignal(false);
  const [notice, setNotice] = createSignal<BannerNotice | undefined>();

  // Auto-dismiss the notice after a delay, but only while no incoming offer
  // is taking the slot. If a new notice replaces an old one, the effect
  // re-runs and the timer resets to give the new notice its full duration.
  let dismissTimer: ReturnType<typeof setTimeout> | undefined;
  const clearDismissTimer = () => {
    if (dismissTimer !== undefined) {
      clearTimeout(dismissTimer);
      dismissTimer = undefined;
    }
  };

  createEffect(
    on(
      () => [notice(), !!incoming()] as const,
      ([n, hasOffer]) => {
        clearDismissTimer();
        if (n && !hasOffer) {
          dismissTimer = setTimeout(
            () => setNotice(undefined),
            NOTICE_AUTO_DISMISS_MS,
          );
        }
      },
    ),
  );

  function clearAll(): void {
    setIncoming(undefined);
    setOutgoingDraw(false);
    setOutgoingUndo(false);
    setNotice(undefined);
  }

  function pushNotice(message: string): void {
    setNotice({ message });
  }

  onMount(() => {
    online.connect(params.id);
  });

  onCleanup(() => {
    clearDismissTimer();
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
          clearAll();
          break;
        case "undoAccepted":
          for (let i = 0; i < event.moveCount; i++) {
            game.undoLastMove();
          }
          setOutgoingUndo(false);
          setIncoming(undefined);
          break;
        case "drawOffer":
          setNotice(undefined); // discard any stale notice
          setIncoming({
            kind: "draw",
            by: opponentName(
              event.by,
              game.store.game.players,
              game.store.game.playerColor,
            ),
          });
          break;
        case "undoRequest":
          setNotice(undefined); // discard any stale notice
          setIncoming({
            kind: "undo",
            by: opponentName(
              event.by,
              game.store.game.players,
              game.store.game.playerColor,
            ),
          });
          break;
        case "drawDeclined":
          setOutgoingDraw(false);
          pushNotice("Draw declined");
          break;
        case "undoDeclined":
          setOutgoingUndo(false);
          pushNotice("Undo declined");
          break;
        case "opponentJoined":
          pushNotice("Opponent joined");
          break;
        case "opponentLeft":
          pushNotice("Opponent disconnected");
          break;
        case "chat":
          // Chat is handled separately (currently no-op; full wiring is a
          // dedicated TODO). Listed here to keep the switch exhaustive.
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
    setOutgoingDraw(true);
  }

  function onUndo() {
    online.requestUndo();
    setOutgoingUndo(true);
  }

  function acceptIncoming() {
    const offer = incoming();
    if (!offer) return;
    if (offer.kind === "draw") online.acceptDraw();
    else online.acceptUndo();
    setIncoming(undefined);
  }

  function declineIncoming() {
    const offer = incoming();
    if (!offer) return;
    if (offer.kind === "draw") online.declineDraw();
    else online.declineUndo();
    setIncoming(undefined);
  }

  return (
    <GameLayout
      mode="online"
      onMove={onMove}
      onResign={onResign}
      onDraw={onDraw}
      onUndo={onUndo}
      connecting={online.connecting()}
      outgoingDrawPending={outgoingDraw()}
      outgoingUndoPending={outgoingUndo()}
      banner={
        <OfferBanner
          pendingOffer={incoming()}
          notice={notice()}
          onAccept={acceptIncoming}
          onDecline={declineIncoming}
          onDismissNotice={() => {
            clearDismissTimer();
            setNotice(undefined);
          }}
        />
      }
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
