import { createEffect, For } from "solid-js";
import { indexToAlgebraic, type Move } from "../board-logic";
import { useGame } from "../game-context";

interface MovePair {
  black: Move;
  white: Move | null;
  startIndex: number;
}

export default function MoveHistory() {
  const game = useGame();
  let scrollRef: HTMLDivElement | undefined;

  const movePairs = (): MovePair[] => {
    const pairs: MovePair[] = [];
    for (let i = 0; i < game.store.game.moveHistory.length; i += 2) {
      pairs.push({
        black: game.store.game.moveHistory[i],
        white: game.store.game.moveHistory[i + 1] || null,
        startIndex: i,
      });
    }
    return pairs;
  };

  createEffect(() => {
    game.currentViewMoveIndex();
    if (scrollRef) {
      const currentEl = scrollRef.querySelector(".current");
      if (currentEl) {
        currentEl.scrollIntoView({ block: "center", behavior: "smooth" });
      }
    }
  });

  const formatMove = (move: Move): string =>
    `${indexToAlgebraic(move.from)}-${indexToAlgebraic(move.to)}`;

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
                  <button
                    type="button"
                    class={
                      blackIdx === game.currentViewMoveIndex() ? "current" : ""
                    }
                    onClick={() => game.jumpToMove(blackIdx)}
                  >
                    {formatMove(pair.black)}
                  </button>
                )}
              </span>
              <span class="move-white">
                {pair.white && (
                  <button
                    type="button"
                    class={
                      whiteIdx === game.currentViewMoveIndex() ? "current" : ""
                    }
                    onClick={() => game.jumpToMove(whiteIdx)}
                  >
                    {formatMove(pair.white)}
                  </button>
                )}
              </span>
            </div>
          );
        }}
      </For>
    </div>
  );
}
