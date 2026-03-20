import { For, createEffect } from "solid-js";
import { store, currentViewMoveIndex, handleJumpToMove } from "../state";
import { indexToAlgebraic, type Move } from "../board-logic";

interface MovePair {
  black: Move;
  white: Move | null;
  startIndex: number;
}

export default function MoveHistory() {
  let scrollRef: HTMLDivElement | undefined;

  const movePairs = (): MovePair[] => {
    const pairs: MovePair[] = [];
    for (let i = 0; i < store.game.moveHistory.length; i += 2) {
      pairs.push({
        black: store.game.moveHistory[i],
        white: store.game.moveHistory[i + 1] || null,
        startIndex: i,
      });
    }
    return pairs;
  };

  createEffect(() => {
    currentViewMoveIndex();
    if (scrollRef) {
      const currentEl = scrollRef.querySelector(".current");
      if (currentEl) {
        currentEl.scrollIntoView({ block: "center", behavior: "smooth" });
      }
    }
  });

  const formatMove = (move: Move): string =>
    indexToAlgebraic(move.from) + "-" + indexToAlgebraic(move.to);

  return (
    <div class="move-history" ref={scrollRef}>
      <For each={movePairs()}>
        {(pair, pairIdx) => {
          const moveNumber = pairIdx() + 1;
          const blackIdx = pair.startIndex;
          const whiteIdx = pair.startIndex + 1;

          return (
            <div class="move-row">
              <span class="move-num">{moveNumber}.</span>
              <span class="move-black">
                {pair.black && (
                  <span
                    class={
                      blackIdx === currentViewMoveIndex() ? "current" : ""
                    }
                    onClick={() => handleJumpToMove(blackIdx)}
                  >
                    {formatMove(pair.black)}
                  </span>
                )}
              </span>
              <span class="move-white">
                {pair.white && (
                  <span
                    class={
                      whiteIdx === currentViewMoveIndex() ? "current" : ""
                    }
                    onClick={() => handleJumpToMove(whiteIdx)}
                  >
                    {formatMove(pair.white)}
                  </span>
                )}
              </span>
            </div>
          );
        }}
      </For>
    </div>
  );
}
