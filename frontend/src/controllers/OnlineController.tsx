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

  // Notices persist until: the user takes a deliberate action, the user
  // clicks the dismiss button, a new incoming offer claims the slot, or
  // the game ends. No timer-based auto-dismiss — async play means the
  // user often isn't watching the screen, and a faded notice is just a
  // missed event.

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
            clock: event.clock,
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
          game.setClock(event.clock);
          clearAll();
          break;
        case "undoAccepted":
          for (let i = 0; i < event.moveCount; i++) {
            game.undoLastMove();
          }
          game.setClock(event.clock);
          setOutgoingUndo(false);
          setIncoming(undefined);
          break;
        case "clockUpdated":
          game.setClock(event.clock);
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
        case "drawCancelled":
          // Server killed our pending draw because the opponent acted
          // (e.g. moved). Mirror the state locally; no notice — the
          // accompanying moveMade event already tells the user what
          // happened.
          setOutgoingDraw(false);
          break;
        case "undoDeclined":
          setOutgoingUndo(false);
          pushNotice("Undo declined");
          break;
        case "undoCancelled":
          setOutgoingUndo(false);
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

  // Any deliberate user action clears the current notice (e.g. a stale
  // "Draw declined" should disappear once the user has moved on).
  function dismissNotice(): void {
    setNotice(undefined);
  }

  function onMove(move: Move) {
    dismissNotice();
    // The server clears the pending-action slot whenever either side
    // moves, regardless of who placed the offer. Mirror locally for both
    // directions — the server doesn't echo cancellations of our own
    // outgoing offers, so without this the UI would think they were
    // still pending and keep the buttons disabled.
    setIncoming(undefined);
    setOutgoingDraw(false);
    setOutgoingUndo(false);
    game.applyMove(move);
    online.sendMove(move);
  }

  function onResign() {
    dismissNotice();
    online.resign();
  }

  // The server keeps a single pending-offer slot per game. Sending an
  // offer (draw or undo) while *any* pending offer exists — ours or the
  // opponent's — produces `action_already_pending`. The Undo/Draw buttons
  // are already disabled in that case, but we guard here too so a stale
  // click event or programmatic call can't slip through.
  const anyOfferPending = () =>
    outgoingDraw() || outgoingUndo() || !!incoming();

  function onDraw() {
    if (anyOfferPending()) return;
    dismissNotice();
    online.offerDraw();
    setOutgoingDraw(true);
  }

  function onUndo() {
    if (anyOfferPending()) return;
    dismissNotice();
    online.requestUndo();
    setOutgoingUndo(true);
  }

  function acceptIncoming() {
    dismissNotice();
    const offer = incoming();
    if (!offer) return;
    if (offer.kind === "draw") online.acceptDraw();
    else online.acceptUndo();
    setIncoming(undefined);
  }

  function declineIncoming() {
    dismissNotice();
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
      incomingOfferPending={!!incoming()}
      banner={
        <OfferBanner
          pendingOffer={incoming()}
          notice={notice()}
          onAccept={acceptIncoming}
          onDecline={declineIncoming}
          onDismissNotice={dismissNotice}
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
