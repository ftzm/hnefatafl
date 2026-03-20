import { createSignal, createMemo, createEffect, For, onMount, onCleanup } from "solid-js";
import {
  store,
  movesDisabled,
  handleMoveMade,
  lastMove,
  pendingAnimation,
  setPendingAnimation,
} from "../state.js";
import { startBoard } from "../board-logic.js";

const corners = new Set([0, 10, 110, 120]);
const center = 60;
const markedSquares = new Set([
  // All starting positions (black + white + center)
  ...startBoard.black,
  ...startBoard.white,
  center,
]);

// Compute arrow rotation based on move direction
// Same geometry as center square chevrons, but filled
function getArrowPoints(from, to) {
  const dx = (to % 11) - (from % 11);
  const dy = Math.floor(to / 11) - Math.floor(from / 11);
  // right: arrow on left edge pointing right
  if (dx > 0) return "0,80 30,50 0,20";
  // left: arrow on right edge pointing left
  if (dx < 0) return "100,20 70,50 100,80";
  // down: arrow on top edge pointing down
  if (dy > 0) return "20,0 50,30 80,0";
  // up: arrow on bottom edge pointing up
  return "80,100 50,70 20,100";
}

export default function Board() {
  const [showingMovesFrom, setShowingMovesFrom] = createSignal(null);

  let drag = null;
  let dragClone = null;
  let selectedOnDown = false;

  let boardRef;
  let wrapperRef;
  let squareRefs = [];

  createEffect(() => {
    const anim = pendingAnimation();
    if (!anim) return;
    setPendingAnimation(null);

    // Clone captured pieces BEFORE state update (they're still in the DOM)
    const captureClones = [];
    if (anim.captures?.length > 0) {
      for (const capIdx of anim.captures) {
        const sq = squareRefs[capIdx];
        const pieceEl = sq?.querySelector(".piece");
        if (pieceEl) {
          const rect = pieceEl.getBoundingClientRect();
          const clone = pieceEl.cloneNode(true);
          clone.style.position = "fixed";
          clone.style.left = rect.left + "px";
          clone.style.top = rect.top + "px";
          clone.style.width = rect.width + "px";
          clone.style.height = rect.height + "px";
          clone.style.zIndex = "999";
          clone.style.pointerEvents = "none";
          document.body.appendChild(clone);
          captureClones.push(clone);
        }
      }
    }

    // Square positions don't change, safe to read before or after state update
    const fromRect = squareRefs[anim.from]?.getBoundingClientRect();

    // Apply deferred state update if provided
    if (anim.applyState) anim.applyState();

    const toRect = squareRefs[anim.to]?.getBoundingClientRect();
    if (fromRect && toRect) {
      const pieceEl = squareRefs[anim.to]?.querySelector(".piece");
      if (pieceEl) {
        const dx = fromRect.left - toRect.left;
        const dy = fromRect.top - toRect.top;
        pieceEl.animate(
          [
            { transform: `translate(${dx}px, ${dy}px)` },
            { transform: "translate(0, 0)" },
          ],
          { duration: 200, easing: "ease" }
        );
      }
    }

    // Animate capture clones (forward nav: pieces disappear)
    for (const clone of captureClones) {
      clone.addEventListener("transitionend", () => clone.remove(), {
        once: true,
      });
      clone.offsetHeight;
      clone.classList.add("capture");
    }

    // Animate restored pieces (backward nav: pieces reappear)
    if (anim.restores?.length > 0) {
      for (const idx of anim.restores) {
        const pieceEl = squareRefs[idx]?.querySelector(".piece");
        if (pieceEl) {
          pieceEl.animate(
            [
              {
                boxShadow: "inset 0 0 0 50px var(--color-capture)",
                opacity: 0,
                transform: "scale(0.85)",
              },
              {
                boxShadow: "inset 0 0 0 0px transparent",
                opacity: 1,
                transform: "scale(1)",
              },
            ],
            { duration: 300, easing: "ease" },
          );
        }
      }
    }
  });

  const highlightedSquares = createMemo(() => {
    const origin = showingMovesFrom();
    if (origin === null || movesDisabled()) return new Set();
    const movesData = store.game.moves[origin];
    if (!movesData) return new Set();
    const destinations = new Set();
    movesData.forEach((move) => {
      const [dest] = Array.isArray(move) ? move : [move];
      destinations.add(dest);
    });
    return destinations;
  });

  const getPieceAt = (index) => {
    if (store.game.boardRep.black.has(index)) return "black";
    if (store.game.boardRep.white.has(index)) return "white";
    if (store.game.boardRep.king === index) return "king";
    return null;
  };

  const getCaptures = (origin, destination) => {
    const movesData = store.game.moves[origin];
    if (!movesData) return [];
    const move = movesData.find((m) => {
      const [dest] = Array.isArray(m) ? m : [m];
      return dest === destination;
    });
    if (!move) return [];
    return Array.isArray(move) ? move.slice(1) : [];
  };

  const executeMove = (move, { animate = true } = {}) => {
    const captureClones = [];
    const fromRect = animate ? squareRefs[move.from]?.getBoundingClientRect() : null;

    if (move.captures && move.captures.length > 0) {
      for (const capIdx of move.captures) {
        const sq = squareRefs[capIdx];
        const pieceEl = sq?.querySelector(".piece");
        if (pieceEl) {
          const rect = pieceEl.getBoundingClientRect();
          const clone = pieceEl.cloneNode(true);
          clone.style.position = "fixed";
          clone.style.left = rect.left + "px";
          clone.style.top = rect.top + "px";
          clone.style.width = rect.width + "px";
          clone.style.height = rect.height + "px";
          clone.style.zIndex = "999";
          clone.style.pointerEvents = "none";
          document.body.appendChild(clone);
          captureClones.push(clone);
        }
      }
    }

    handleMoveMade(move);

    // Animate the moved piece sliding from origin to destination (skip for drag-drop)
    if (animate) {
      const toRect = squareRefs[move.to]?.getBoundingClientRect();
      if (fromRect && toRect) {
        const pieceEl = squareRefs[move.to]?.querySelector(".piece");
        if (pieceEl) {
          const dx = fromRect.left - toRect.left;
          const dy = fromRect.top - toRect.top;
          pieceEl.animate(
            [
              { transform: `translate(${dx}px, ${dy}px)` },
              { transform: "translate(0, 0)" },
            ],
            { duration: 300, easing: "ease" }
          );
        }
      }
    }

    // Fade out captured piece clones
    for (const clone of captureClones) {
      clone.addEventListener("transitionend", () => clone.remove(), {
        once: true,
      });
      clone.offsetHeight;
      clone.classList.add("capture");
    }
  };

  const handleSquareClick = (index) => {
    if (drag) return;

    const highlighted = highlightedSquares();
    if (highlighted.has(index)) {
      const origin = showingMovesFrom();
      if (origin === null) return;
      const captures = getCaptures(origin, index);
      const move = { from: origin, to: index, captures };
      setShowingMovesFrom(null);
      executeMove(move);
      return;
    }

    const piece = getPieceAt(index);
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

  const handlePointerDown = (index, e) => {
    const piece = getPieceAt(index);
    if (!piece || movesDisabled()) return;
    if (!store.game.moves[index]) return;

    const squareEl = squareRefs[index];
    if (!squareEl) return;
    const pieceEl = squareEl.querySelector(".piece");
    if (!pieceEl) return;

    const rect = pieceEl.getBoundingClientRect();

    if (showingMovesFrom() !== index) {
      setShowingMovesFrom(index);
      selectedOnDown = true;
    }

    drag = {
      pieceIndex: index,
      originalPiece: pieceEl,
      width: rect.width,
      height: rect.height,
      hasMoved: false,
      pointerId: e.pointerId,
    };
  };

  const handlePointerMove = (e) => {
    if (!drag) return;
    e.preventDefault();

    if (!drag.hasMoved) {
      drag.hasMoved = true;
      boardRef.setPointerCapture(drag.pointerId);
      drag.originalPiece.style.visibility = "hidden";

      dragClone = drag.originalPiece.cloneNode(true);
      dragClone.style.position = "fixed";
      dragClone.style.left = "0";
      dragClone.style.top = "0";
      dragClone.style.zIndex = "1000";
      dragClone.style.pointerEvents = "none";
      dragClone.style.width = drag.width + "px";
      dragClone.style.height = drag.height + "px";
      dragClone.style.visibility = "visible";
      dragClone.style.willChange = "transform";
      dragClone.style.boxShadow = "0 4px 8px rgba(0, 0, 0, 0.5)";
      document.body.appendChild(dragClone);

      document.body.classList.add("dragging-piece");
      setShowingMovesFrom(drag.pieceIndex);
    }

    const x = e.clientX - drag.width / 2;
    const y = e.clientY - drag.height / 2;
    dragClone.style.transform = `translate(${x}px, ${y}px) scale(1.05)`;
  };

  const cleanupDrag = () => {
    if (dragClone) {
      dragClone.remove();
      dragClone = null;
    }
    if (drag && drag.originalPiece) {
      drag.originalPiece.style.visibility = "";
    }
    if (drag && drag.hasMoved) {
      boardRef.releasePointerCapture(drag.pointerId);
    }
    document.body.classList.remove("dragging-piece");
  };

  const handlePointerUp = (e) => {
    if (!drag) return;
    e.preventDefault();

    const wasDragging = drag.hasMoved;
    cleanupDrag();

    if (wasDragging) {
      const targetSquare = getSquareUnderMouse(e);
      if (targetSquare !== null && highlightedSquares().has(targetSquare)) {
        const captures = getCaptures(drag.pieceIndex, targetSquare);
        const move = { from: drag.pieceIndex, to: targetSquare, captures };
        setShowingMovesFrom(null);
        drag = null;
        executeMove(move, { animate: false });
        return;
      }
      setShowingMovesFrom(null);
    }

    drag = null;
  };

  const getSquareUnderMouse = (e) => {
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

  const updateLineWidth = () => {
    if (wrapperRef) {
      const size = wrapperRef.offsetWidth;
      const lineW = Math.max(1, Math.round(size / 11 / 70));
      wrapperRef.style.setProperty("--line-w", lineW + "px");
    }
  };

  onMount(() => {
    boardRef.addEventListener("pointermove", handlePointerMove);
    boardRef.addEventListener("pointerup", handlePointerUp);
    updateLineWidth();
    window.addEventListener("resize", updateLineWidth);
  });

  onCleanup(() => {
    boardRef.removeEventListener("pointermove", handlePointerMove);
    boardRef.removeEventListener("pointerup", handlePointerUp);
    window.removeEventListener("resize", updateLineWidth);
  });

  return (
    <div class="board-wrapper" ref={wrapperRef}>
      <div class="board" ref={boardRef}>
        <For each={Array.from({ length: 121 }, (_, i) => i)}>
          {(index) => {
            const piece = () => getPieceAt(index);
            const isValidMove = () => highlightedSquares().has(index);
            const isLastMoveFrom = () => {
              const lm = lastMove();
              return lm && lm.from === index;
            };
            const isSelected = () =>
              showingMovesFrom() === index && piece();

            const squareClass = () => {
              let cls = "square";
              if (corners.has(index)) cls += " corner";
              else if (index === center) cls += " center";
              else if (markedSquares.has(index)) cls += " marked";
              if (isValidMove()) cls += " valid-move";
              if (isLastMoveFrom()) cls += " last-move-from";
              return cls;
            };

            const pieceClass = () => {
              const p = piece();
              if (!p) return "";
              let cls = `piece ${p}`;
              if (isSelected()) cls += " selected";
              return cls;
            };

            return (
              <div
                class={squareClass()}
                data-index={index}
                ref={(el) => (squareRefs[index] = el)}
                onClick={() => handleSquareClick(index)}
                on:pointerdown={(e) => handlePointerDown(index, e)}
              >
                <div class="inner">
                  {(markedSquares.has(index) && index !== center) && (
                    <svg viewBox="0 0 100 100" preserveAspectRatio="none">
                      <line x1="0" y1="0" x2="100" y2="100" stroke="var(--board-square-inner)" stroke-width="2" />
                      <line x1="100" y1="0" x2="0" y2="100" stroke="var(--board-square-inner)" stroke-width="2" />
                    </svg>
                  )}
                  {corners.has(index) && (
                    <svg viewBox="0 0 100 100" preserveAspectRatio="none">
                      <line x1="0" y1="0" x2="100" y2="100" stroke="var(--board-corner-border)" stroke-width="2" />
                      <line x1="100" y1="0" x2="0" y2="100" stroke="var(--board-corner-border)" stroke-width="2" />
                    </svg>
                  )}
                  {index === center && (
                    <svg viewBox="0 0 100 100" preserveAspectRatio="none">
                      <line x1="0" y1="0" x2="100" y2="100" stroke="var(--board-square-inner)" stroke-width="2" />
                      <line x1="100" y1="0" x2="0" y2="100" stroke="var(--board-square-inner)" stroke-width="2" />
                      <polyline points="20,0 50,30 80,0" fill="none" stroke="var(--board-square-inner)" stroke-width="2" />
                      <polyline points="100,20 70,50 100,80" fill="none" stroke="var(--board-square-inner)" stroke-width="2" />
                      <polyline points="80,100 50,70 20,100" fill="none" stroke="var(--board-square-inner)" stroke-width="2" />
                      <polyline points="0,80 30,50 0,20" fill="none" stroke="var(--board-square-inner)" stroke-width="2" />
                    </svg>
                  )}
                  {isLastMoveFrom() && (
                    <svg class="move-arrow" viewBox="0 0 100 100" preserveAspectRatio="none">
                      <polygon
                        points={getArrowPoints(lastMove().from, lastMove().to)}
                        fill="rgba(0,0,0,0.15)"
                      />
                    </svg>
                  )}
                </div>
                {piece() && (
                  <div class={pieceClass()}>
                    {piece() === "king" && (
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
                )}
              </div>
            );
          }}
        </For>
      </div>
    </div>
  );
}
