import Board from "./components/Board.jsx";
import Chat from "./components/Chat.jsx";
import Controls from "./components/Controls.jsx";
import PlayerBar from "./components/PlayerBar.jsx";
import Button from "./components/ui/Button.jsx";
import { store } from "./state.js";

export default function App() {
  const isBlackActive = () =>
    store.currentPlayer === "black" && store.historyCursor === 0;
  const isWhiteActive = () =>
    store.currentPlayer === "white" && store.historyCursor === 0;

  return (
    <>
      <div class="top-bar">
        <span class="logo">HNEFATAFL</span>
        <nav class="nav-links">
          <a href="#" class="active">
            Play
          </a>
          <a href="#">Games</a>
        </nav>
        <div class="nav-right">
          <Button variant="ghost" aria-label="Settings">
            <svg viewBox="0 0 24 24">
              <path d="M12 15.5A3.5 3.5 0 0 1 8.5 12 3.5 3.5 0 0 1 12 8.5a3.5 3.5 0 0 1 3.5 3.5 3.5 3.5 0 0 1-3.5 3.5m7.43-2.53a7.76 7.76 0 0 0 0-1.94l2.11-1.65a.5.5 0 0 0 .12-.64l-2-3.46a.5.5 0 0 0-.61-.22l-2.49 1a7.3 7.3 0 0 0-1.69-.98l-.38-2.65A.49.49 0 0 0 14 2h-4a.49.49 0 0 0-.49.42l-.38 2.65a7.3 7.3 0 0 0-1.69.98l-2.49-1a.5.5 0 0 0-.61.22l-2 3.46a.49.49 0 0 0 .12.64l2.11 1.65a7.93 7.93 0 0 0 0 1.94l-2.11 1.65a.5.5 0 0 0-.12.64l2 3.46a.5.5 0 0 0 .61.22l2.49-1c.52.4 1.08.72 1.69.98l.38 2.65c.05.24.26.42.49.42h4c.24 0 .44-.18.49-.42l.38-2.65a7.3 7.3 0 0 0 1.69-.98l2.49 1a.5.5 0 0 0 .61-.22l2-3.46a.49.49 0 0 0-.12-.64l-2.11-1.65Z" />
            </svg>
          </Button>
        </div>
      </div>
      <div class="page-content">
        <div class="main-layout">
          <div class="board-section">
            <PlayerBar
              color="black"
              capturedCount={store.capturedPieces.white}
              active={isBlackActive()}
            />
            <Board />
            <PlayerBar
              color="white"
              capturedCount={store.capturedPieces.black}
              active={isWhiteActive()}
            />
          </div>
          <div class="right-column">
            <div class="side-panel">
              <Controls />
            </div>
            <Chat />
          </div>
        </div>
      </div>
    </>
  );
}
