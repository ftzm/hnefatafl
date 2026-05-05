import {
  createEffect,
  createMemo,
  createSignal,
  For,
  onCleanup,
  onMount,
  untrack,
} from "solid-js";
import { createStore, produce } from "solid-js/store";
import {
  type Move,
  type Piece,
  type PieceKind,
  startBoard,
} from "../board-logic";
import { useGame } from "../game-context";

const corners = new Set([0, 10, 110, 120]);
const center = 60;
const markedSquares = new Set([
  ...startBoard.black,
  ...startBoard.white,
  center,
]);

// How long captured pieces linger in the DOM playing their exit animation.
// Keep in sync with the longest transition on `.piece-slot.exiting .piece`
// in `styles.css` (currently `transform 0.4s`).
const EXIT_DURATION_MS = 400;

// How long the `.entering` class lingers on a restored piece while its
// fade-in animation plays. Keep in sync with the `piece-restore` keyframe
// duration in `styles.css`.
const ENTER_DURATION_MS = 300;

function getArrowPoints(from: number, to: number): string {
  const dx = (to % 11) - (from % 11);
  const dy = Math.floor(to / 11) - Math.floor(from / 11);
  if (dx > 0) return "0,80 30,50 0,20";
  if (dx < 0) return "100,20 70,50 100,80";
  if (dy > 0) return "20,0 50,30 80,0";
  return "80,100 50,70 20,100";
}

interface DragState {
  pieceId: string;
  fromSquare: number;
  slotEl: HTMLElement;
  pieceEl: HTMLElement;
  width: number;
  height: number;
  hasMoved: boolean;
  pointerId: number;
}

/**
 * A piece as held by the renderer. Mirrors `Piece` from the game model but
 * adds animation flags:
 *
 *   `exiting`  — keep the piece in the DOM long enough to play its capture
 *                animation before we remove it.
 *   `entering` — the piece just appeared as a *restoration* (the user
 *                navigated backward past a capture move), so the renderer
 *                should play the fade-in/uncapture animation on it. Cleared
 *                after the animation finishes; not set on initial population
 *                or any other reason a piece can appear.
 */
interface SlotPiece {
  id: string;
  kind: PieceKind;
  square: number;
  exiting: boolean;
  entering: boolean;
}

interface BoardProps {
  onMove: (move: Move) => void;
}

export default function Board(props: BoardProps) {
  const game = useGame();
  const { store, movesDisabled, lastMove } = game;
  const [showingMovesFrom, setShowingMovesFrom] = createSignal<number | null>(
    null,
  );

  // === Slot state (renderer-owned) ============================================
  //
  // `game.pieces()` is a pure derived view of the game model — every call
  // produces a *new* array of *new* objects. Solid's `<For>` keys by reference
  // by default, so feeding it that array directly would tear down and rebuild
  // every piece DOM node on every move, defeating CSS transitions.
  //
  // We instead maintain our own `slots` store: a stable array of mutable
  // entries keyed by piece id. The reconciliation effect below mutates entries
  // in place when the game model changes, which is what `<For>` needs to reuse
  // DOM nodes (so the *same* `.piece-slot` element transitions its transform
  // when its piece's square changes).
  //
  // The `exiting` flag lets us keep a piece in the DOM after the model says
  // it's gone, so its capture animation can play out. A timer cleans it up.
  const [slots, setSlots] = createStore<SlotPiece[]>([]);
  const slotRefs = new Map<string, HTMLElement>();

  // Tracks the previous reconciliation's cursor + history length so we can
  // tell, when a piece appears in `target`, *why* it appeared:
  //   - cursor went up while history length was unchanged → backward nav
  //     (the only case where we play the restore-fade animation)
  //   - any other delta → initial population, applyMove, undoLastMove,
  //     initGame, etc. — none of which should animate appearances.
  let prevCursor = store.game.historyCursor;
  let prevHistoryLen = store.game.moveHistory.length;

  /** Clear `entering` after the restore animation has finished playing. */
  const scheduleClearEntering = (id: string) => {
    setTimeout(() => {
      setSlots(
        produce((arr) => {
          const j = arr.findIndex((x) => x.id === id);
          if (j !== -1) arr[j].entering = false;
        }),
      );
    }, ENTER_DURATION_MS);
  };

  createEffect(() => {
    const target = game.pieces();
    const cursor = store.game.historyCursor;
    const historyLen = store.game.moveHistory.length;
    const isBackwardNav = cursor > prevCursor && historyLen === prevHistoryLen;
    prevCursor = cursor;
    prevHistoryLen = historyLen;

    // All store mutations below are deliberately *not* reactive reads of
    // `slots` — we'd self-trigger if they were. `untrack` makes that explicit.
    untrack(() => {
      const targetIds = new Set(target.map((p) => p.id));

      // 1. Update existing slots / append brand-new ones.
      //    Doing this before the exit-marking pass keeps array indices stable
      //    for the index-based `setSlots(i, ...)` writes here.
      for (const t of target) {
        const idx = slots.findIndex((s) => s.id === t.id);
        if (idx === -1) {
          // Piece is appearing. Mark `entering` only when this appearance is
          // a restoration (backward navigation past a capture).
          const entering = isBackwardNav;
          setSlots(
            produce((arr) => {
              arr.push({ ...t, exiting: false, entering });
            }),
          );
          if (entering) scheduleClearEntering(t.id);
        } else {
          if (slots[idx].square !== t.square) {
            setSlots(idx, "square", t.square);
          }
          // A piece can come "back" while still mid-exit if the user
          // navigates backward through history during the exit animation
          // window. Cancel the exit and play the restore-fade if it's a
          // backward nav.
          if (slots[idx].exiting) {
            setSlots(idx, "exiting", false);
            if (isBackwardNav) {
              setSlots(idx, "entering", true);
              scheduleClearEntering(slots[idx].id);
            }
          }
        }
      }

      // 2. Mark slots whose pieces have left the model as exiting, and
      //    schedule their removal once the animation finishes.
      for (let i = 0; i < slots.length; i++) {
        const s = slots[i];
        if (!targetIds.has(s.id) && !s.exiting) {
          setSlots(i, "exiting", true);
          const id = s.id;
          setTimeout(() => {
            setSlots(
              produce((arr) => {
                const j = arr.findIndex((x) => x.id === id);
                // The slot may have been resurrected (exit cancelled) before
                // the timer fired — only remove if it is *still* exiting.
                if (j !== -1 && arr[j].exiting) arr.splice(j, 1);
              }),
            );
          }, EXIT_DURATION_MS);
        }
      }
    });
  });

  // === Drag/drop ============================================================

  let drag: DragState | null = null;
  let dragClone: HTMLElement | null = null;
  let selectedOnDown = false;

  let boardRef: HTMLDivElement | undefined;
  let wrapperRef: HTMLDivElement | undefined;
  const squareRefs: Array<HTMLDivElement | undefined> = [];

  const highlightedSquares = createMemo(() => {
    const origin = showingMovesFrom();
    if (origin === null || movesDisabled()) return new Set<number>();
    const movesData = store.game.moves[origin];
    if (!movesData) return new Set<number>();
    const destinations = new Set<number>();
    for (const move of movesData) destinations.add(move.to);
    return destinations;
  });

  /** Find the live (non-exiting) piece occupying the given square, if any. */
  const livePieceAt = (square: number): SlotPiece | undefined =>
    slots.find((s) => !s.exiting && s.square === square);

  const getCaptures = (origin: number, destination: number): number[] => {
    const movesData = store.game.moves[origin];
    if (!movesData) return [];
    const move = movesData.find((m) => m.to === destination);
    return move ? move.captures : [];
  };

  /**
   * Apply a move from a click. The state mutation triggers the reconciliation
   * effect, which updates the moving piece's `square`. The CSS transition on
   * `.piece-slot` then animates from old to new position automatically.
   * Captures vanish via the `exiting` flag + keyframe.
   */
  const applyClickMove = (move: Move) => {
    setShowingMovesFrom(null);
    props.onMove(move);
  };

  /**
   * Apply a move from a drag-drop. Same as a click move, except we want the
   * slot to *teleport* to its destination (the user has already visually
   * dragged it there with the clone) instead of sliding 200ms across the
   * board. We do that by suppressing the slot's transition for one frame.
   */
  const applyDragDropMove = (move: Move, slotEl: HTMLElement) => {
    // Suppress the upcoming transform change driven by the model update.
    slotEl.style.transition = "none";
    setShowingMovesFrom(null);
    props.onMove(move);
    // Force the browser to apply the new transform with no transition…
    void slotEl.offsetHeight;
    // …then hand control back to the CSS rule for future moves.
    slotEl.style.transition = "";
  };

  const handleSquareClick = (index: number) => {
    if (drag) return;

    const highlighted = highlightedSquares();
    if (highlighted.has(index)) {
      const origin = showingMovesFrom();
      if (origin === null) return;
      applyClickMove({
        from: origin,
        to: index,
        captures: getCaptures(origin, index),
      });
      return;
    }

    const piece = livePieceAt(index);
    if (piece && store.game.moves[index] && !movesDisabled()) {
      if (selectedOnDown) {
        selectedOnDown = false;
      } else if (showingMovesFrom() === index) {
        setShowingMovesFrom(null);
      } else {
        setShowingMovesFrom(index);
      }
    } else {
      setShowingMovesFrom(null);
    }
  };

  const handlePointerDown = (index: number, e: PointerEvent) => {
    const piece = livePieceAt(index);
    if (!piece || movesDisabled()) return;
    if (!store.game.moves[index]) return;

    const slotEl = slotRefs.get(piece.id);
    if (!slotEl) return;
    const pieceEl = slotEl.querySelector(".piece") as HTMLElement | null;
    if (!pieceEl) return;
    const rect = pieceEl.getBoundingClientRect();

    if (showingMovesFrom() !== index) {
      setShowingMovesFrom(index);
      selectedOnDown = true;
    }

    drag = {
      pieceId: piece.id,
      fromSquare: index,
      slotEl,
      pieceEl,
      width: rect.width,
      height: rect.height,
      hasMoved: false,
      pointerId: e.pointerId,
    };
  };

  const handlePointerMove = (e: PointerEvent) => {
    if (!drag) return;
    e.preventDefault();

    if (!drag.hasMoved) {
      drag.hasMoved = true;
      boardRef?.setPointerCapture(drag.pointerId);
      // Hide the live piece while a clone follows the cursor. The clone is
      // the "drag avatar" — a position:fixed copy mutated imperatively per
      // pointermove, deliberately bypassing Solid's reactive system to avoid
      // re-rendering the board on every mouse delta.
      drag.pieceEl.style.visibility = "hidden";

      dragClone = drag.pieceEl.cloneNode(true) as HTMLElement;
      dragClone.style.position = "fixed";
      dragClone.style.left = "0";
      dragClone.style.top = "0";
      dragClone.style.zIndex = "1000";
      dragClone.style.pointerEvents = "none";
      // `getBoundingClientRect()` returns border-box dimensions; match that
      // here so the clone's rendered size equals the original's exactly
      // (otherwise default content-box would inflate the clone by the
      // border thickness on each side).
      dragClone.style.boxSizing = "border-box";
      dragClone.style.width = `${drag.width}px`;
      dragClone.style.height = `${drag.height}px`;
      dragClone.style.visibility = "visible";
      dragClone.style.willChange = "transform";
      dragClone.style.boxShadow = "0 4px 8px rgba(0, 0, 0, 0.5)";
      // Append to `.board-wrapper` (boardRef.parentElement) rather than
      // `document.body` so the clone inherits `--line-w` — the CSS custom
      // property the piece's `border: calc(var(--line-w) * 2) solid …`
      // depends on. Outside `.board-wrapper` that variable is undefined,
      // the calc resolves to a guaranteed-invalid value, and the entire
      // `border` declaration falls back to its initial (none) — i.e. the
      // border visibly disappears the moment you pick up a piece.
      // `position: fixed` is unaffected by DOM parent here because
      // `.board-wrapper` doesn't establish a containing block (no
      // transform/filter/perspective/contain).
      (boardRef?.parentElement ?? document.body).appendChild(dragClone);

      document.body.classList.add("dragging-piece");
      setShowingMovesFrom(drag.fromSquare);
    }

    const x = e.clientX - drag.width / 2;
    const y = e.clientY - drag.height / 2;
    if (dragClone) {
      dragClone.style.transform = `translate(${x}px, ${y}px) scale(1.05)`;
    }
  };

  const cleanupDrag = () => {
    if (dragClone) {
      dragClone.remove();
      dragClone = null;
    }
    if (drag?.pieceEl) {
      drag.pieceEl.style.visibility = "";
    }
    if (drag?.hasMoved) {
      boardRef?.releasePointerCapture(drag.pointerId);
    }
    document.body.classList.remove("dragging-piece");
  };

  const handlePointerUp = (e: PointerEvent) => {
    if (!drag) return;
    e.preventDefault();

    const wasDragging = drag.hasMoved;
    const fromSquare = drag.fromSquare;
    const slotEl = drag.slotEl;
    cleanupDrag();

    if (wasDragging) {
      const targetSquare = getSquareUnderMouse(e);
      if (targetSquare !== null && highlightedSquares().has(targetSquare)) {
        applyDragDropMove(
          {
            from: fromSquare,
            to: targetSquare,
            captures: getCaptures(fromSquare, targetSquare),
          },
          slotEl,
        );
        drag = null;
        return;
      }
      setShowingMovesFrom(null);
    }

    drag = null;
  };

  const getSquareUnderMouse = (e: PointerEvent): number | null => {
    for (let i = 0; i < squareRefs.length; i++) {
      const sq = squareRefs[i];
      if (!sq) continue;
      const rect = sq.getBoundingClientRect();
      if (
        e.clientX >= rect.left &&
        e.clientX <= rect.right &&
        e.clientY >= rect.top &&
        e.clientY <= rect.bottom
      ) {
        return i;
      }
    }
    return null;
  };

  /**
   * Update the two CSS variables that the board geometry depends on:
   *   --line-w     pixel width of grid lines (visual)
   *   --cell-size  pixel side-length of one cell (positioning math)
   *
   * Piece slots are absolutely positioned relative to the .board's *padding
   * edge* (inside its border), so we measure the inner width with
   * `clientWidth`, not `offsetWidth`. `offsetWidth` would include the border
   * and produce a cell-size that's slightly too large, drifting pieces
   * down-and-right by `border × col` / `border × row`.
   *
   * `--line-w` is still derived from `offsetWidth` because that ratio is just
   * a scale factor (~1px per 70px of board) and doesn't affect positioning.
   *
   * Note: this is intentionally NOT rounded. An earlier `Math.round(...)`
   * produced visible 1→2px jumps in line thickness at cell-size 105 (board
   * ~1155px) and 175 (~1925px). Those jumps were especially noticeable at
   * the mobile↔stacked breakpoint, where the two layouts use different
   * `--board-fit` formulas and the board itself resizes discontinuously
   * across the boundary — straddling the `cell = 105` threshold flipped
   * lineW by 100% in a single frame. With a continuous expression the
   * transition is proportional and barely perceptible. Sub-pixel borders
   * anti-alias slightly on 1x DPR but render crisply on high-DPI; for
   * small boards (cell ≤ 70) the `Math.max(1, …)` clamp keeps lineW at
   * exactly 1px, preserving a crisp grid where it matters most.
   */
  /**
   * Resize causes `--cell-size` to change, which changes every piece slot's
   * resolved `transform`. Without intervention the `transition: transform`
   * on `.piece-slot` would happily animate that change too — so every piece
   * would visibly slide whenever the user resized the window or rotated
   * their phone. We add `.resizing` while updating the vars, which
   * suppresses the slot transition for that frame, then drop it on the
   * next animation frame.
   */
  const updateLayoutVars = () => {
    if (!boardRef) return;
    const lineW = Math.max(1, boardRef.offsetWidth / 11 / 70);
    boardRef.classList.add("resizing");
    boardRef.parentElement?.style.setProperty("--line-w", `${lineW}px`);
    boardRef.style.setProperty("--cell-size", `${boardRef.clientWidth / 11}px`);
    // Force the browser to commit the new computed transform with the
    // transition disabled before we hand control back.
    void boardRef.offsetHeight;
    requestAnimationFrame(() => {
      boardRef?.classList.remove("resizing");
    });
  };

  onMount(() => {
    boardRef?.addEventListener("pointermove", handlePointerMove);
    boardRef?.addEventListener("pointerup", handlePointerUp);
    updateLayoutVars();
    const ro = new ResizeObserver(() => updateLayoutVars());
    if (boardRef) ro.observe(boardRef);
    onCleanup(() => {
      boardRef?.removeEventListener("pointermove", handlePointerMove);
      boardRef?.removeEventListener("pointerup", handlePointerUp);
      ro.disconnect();
    });
  });

  return (
    <div class="board-wrapper" ref={wrapperRef}>
      <div class="board" ref={boardRef}>
        {/* === Squares: grid cells, decorations, highlights, last-move arrow.
            Pieces are *not* children of squares — they live in the sibling
            <For each={slots}> below, absolutely positioned over the grid. */}
        <For each={Array.from({ length: 121 }, (_, i) => i)}>
          {(index) => {
            const isValidMove = () => highlightedSquares().has(index);
            const isLastMoveFrom = () => {
              const lm = lastMove();
              return lm && lm.from === index;
            };

            const squareClass = () => {
              let cls = "square";
              if (corners.has(index)) cls += " corner";
              else if (index === center) cls += " center";
              else if (markedSquares.has(index)) cls += " marked";
              if (isValidMove()) cls += " valid-move";
              if (isLastMoveFrom()) cls += " last-move-from";
              return cls;
            };

            return (
              <div
                class={squareClass()}
                data-index={index}
                ref={(el) => {
                  squareRefs[index] = el;
                }}
                onClick={() => handleSquareClick(index)}
                on:pointerdown={(e) => handlePointerDown(index, e)}
              >
                <div class="inner">
                  {markedSquares.has(index) && index !== center && (
                    <svg viewBox="0 0 100 100" preserveAspectRatio="none">
                      <line
                        x1="0"
                        y1="0"
                        x2="100"
                        y2="100"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                      <line
                        x1="100"
                        y1="0"
                        x2="0"
                        y2="100"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                    </svg>
                  )}
                  {corners.has(index) && (
                    <svg viewBox="0 0 100 100" preserveAspectRatio="none">
                      <line
                        x1="0"
                        y1="0"
                        x2="100"
                        y2="100"
                        stroke="var(--board-corner-border)"
                        stroke-width="2"
                      />
                      <line
                        x1="100"
                        y1="0"
                        x2="0"
                        y2="100"
                        stroke="var(--board-corner-border)"
                        stroke-width="2"
                      />
                    </svg>
                  )}
                  {index === center && (
                    <svg viewBox="0 0 100 100" preserveAspectRatio="none">
                      <line
                        x1="0"
                        y1="0"
                        x2="100"
                        y2="100"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                      <line
                        x1="100"
                        y1="0"
                        x2="0"
                        y2="100"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                      <polyline
                        points="20,0 50,30 80,0"
                        fill="none"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                      <polyline
                        points="100,20 70,50 100,80"
                        fill="none"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                      <polyline
                        points="80,100 50,70 20,100"
                        fill="none"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                      <polyline
                        points="0,80 30,50 0,20"
                        fill="none"
                        stroke="var(--board-square-inner)"
                        stroke-width="2"
                      />
                    </svg>
                  )}
                  {(() => {
                    const lm = isLastMoveFrom() ? lastMove() : null;
                    if (!lm) return null;
                    return (
                      <svg
                        class="move-arrow"
                        viewBox="0 0 100 100"
                        preserveAspectRatio="none"
                      >
                        <polygon
                          points={getArrowPoints(lm.from, lm.to)}
                          fill="rgba(0,0,0,0.15)"
                        />
                      </svg>
                    );
                  })()}
                </div>
              </div>
            );
          }}
        </For>

        {/* === Pieces: a flat keyed list, each absolutely positioned within
            the board via CSS transforms driven by --col / --row. Movement
            animation is the CSS `transition: transform` on .piece-slot,
            applied automatically when those vars change. Capture animation
            is the `.piece-slot.exiting` keyframe. No JS animation code. */}
        <For each={slots}>
          {(slot) => {
            const col = () => slot.square % 11;
            const row = () => Math.floor(slot.square / 11);
            const isSelected = () =>
              !slot.exiting && showingMovesFrom() === slot.square;
            const slotClass = () => {
              let c = "piece-slot";
              if (slot.exiting) c += " exiting";
              if (slot.entering) c += " entering";
              return c;
            };
            const pieceClass = () => {
              let c = `piece ${slot.kind}`;
              if (isSelected()) c += " selected";
              return c;
            };
            return (
              <div
                class={slotClass()}
                data-piece-id={slot.id}
                data-square={slot.square}
                style={{
                  "--col": `${col()}`,
                  "--row": `${row()}`,
                }}
                ref={(el) => {
                  slotRefs.set(slot.id, el);
                  onCleanup(() => slotRefs.delete(slot.id));
                }}
              >
                <div class={pieceClass()}>
                  {slot.kind === "king" && (
                    <svg viewBox="0 0 100 100">
                      <circle
                        cx="50"
                        cy="50"
                        r="36.9"
                        fill="none"
                        stroke="rgba(0,0,0,0.55)"
                        stroke-width="2"
                      />
                      <circle
                        cx="50"
                        cy="50"
                        r="23.8"
                        fill="none"
                        stroke="rgba(0,0,0,0.55)"
                        stroke-width="2"
                      />
                      <circle
                        cx="50"
                        cy="50"
                        r="10.6"
                        fill="rgba(0,0,0,0.55)"
                        stroke="none"
                      />
                    </svg>
                  )}
                </div>
              </div>
            );
          }}
        </For>
      </div>
    </div>
  );
}

// `Piece` is re-exported to keep imports tidy for callers that want the type.
export type { Piece };
