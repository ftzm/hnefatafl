import {
  canViewPrev,
  canViewNext,
  handleViewStart,
  handleViewPreviousMove,
  handleViewNextMove,
  handleViewEnd,
  store,
} from "../state.js";
import MoveHistory from "./MoveHistory.jsx";
import Button from "./ui/Button.jsx";
import ButtonGroup from "./ui/ButtonGroup.jsx";
import Toolbar from "./ui/Toolbar.jsx";

export default function Controls() {
  const statusText = () => {
    const player = store.currentPlayer === "black" ? "Black" : "White";
    return `${player} is thinking`;
  };

  return (
    <>
      <div class="game-status">
        {statusText()}
        <span class="thinking-dots">
          <span>.</span>
          <span>.</span>
          <span>.</span>
        </span>
      </div>
      <div class="move-section">
        <MoveHistory />
        <Toolbar aria-label="Move navigation">
          <Button disabled={!canViewPrev()} onClick={handleViewStart}>
            <svg viewBox="0 0 16 16">
              <rect x="2" y="3" width="2" height="10" />
              <polygon points="14,3 6,8 14,13" />
            </svg>
          </Button>
          <Button disabled={!canViewPrev()} onClick={handleViewPreviousMove}>
            <svg viewBox="0 0 16 16">
              <polygon points="12,3 4,8 12,13" />
            </svg>
          </Button>
          <Button disabled={!canViewNext()} onClick={handleViewNextMove}>
            <svg viewBox="0 0 16 16">
              <polygon points="4,3 12,8 4,13" />
            </svg>
          </Button>
          <Button disabled={!canViewNext()} onClick={handleViewEnd}>
            <svg viewBox="0 0 16 16">
              <polygon points="2,3 10,8 2,13" />
              <rect x="12" y="3" width="2" height="10" />
            </svg>
          </Button>
        </Toolbar>
      </div>
      <ButtonGroup>
        <Button>New Game</Button>
        <Button>Undo</Button>
        <Button>Resign</Button>
      </ButtonGroup>
    </>
  );
}
