import { For, createSignal } from "solid-js";
import GameCard from "../components/GameCard.jsx";
import HotseatSetupModal from "../components/HotseatSetupModal.jsx";
import AiSetupModal from "../components/AiSetupModal.jsx";
import OnlineSetupModal from "../components/OnlineSetupModal.jsx";
import { mockInProgressGames, mockCompletedGames } from "../mock-data.js";

export default function Home() {
  const [hotseatOpen, setHotseatOpen] = createSignal(false);
  const [aiOpen, setAiOpen] = createSignal(false);
  const [onlineOpen, setOnlineOpen] = createSignal(false);

  return (
    <div class="home-page">
      <div class="home-hero">
        <h1>Hnefatafl</h1>
        <p>The Viking board game of strategy</p>
      </div>

      <HotseatSetupModal open={hotseatOpen()} onOpenChange={setHotseatOpen} />
      <AiSetupModal open={aiOpen()} onOpenChange={setAiOpen} />
      <OnlineSetupModal open={onlineOpen()} onOpenChange={setOnlineOpen} />

      <div class="home-actions">
        <div class="action-card primary" onClick={() => setHotseatOpen(true)}>
          <span class="action-label">Play Hotseat</span>
          <span class="action-desc">Two players, one screen</span>
        </div>
        <div class="action-card primary" onClick={() => setAiOpen(true)}>
          <span class="action-label">Play vs AI</span>
          <span class="action-desc">Challenge the computer</span>
        </div>
        <div class="action-card primary" onClick={() => setOnlineOpen(true)}>
          <span class="action-label">Play Online</span>
          <span class="action-desc">Find an opponent</span>
        </div>
      </div>

      <section class="home-section">
        <h2>In Progress</h2>
        <div class="game-list">
          <For each={mockInProgressGames}>
            {(game) => <GameCard game={game} />}
          </For>
        </div>
      </section>

      <section class="home-section">
        <h2>Recent Games</h2>
        <div class="game-list">
          <For each={mockCompletedGames}>
            {(game) => <GameCard game={game} />}
          </For>
        </div>
      </section>
    </div>
  );
}
