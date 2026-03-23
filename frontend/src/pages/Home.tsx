import { createSignal, For } from "solid-js";
import AiSetupModal from "../components/AiSetupModal";
import GameCard from "../components/GameCard";
import HotseatSetupModal from "../components/HotseatSetupModal";
import OnlineSetupModal from "../components/OnlineSetupModal";
import { mockCompletedGames, mockInProgressGames } from "../mocks/mock-data";

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
        <button
          type="button"
          class="action-card primary"
          onClick={() => setHotseatOpen(true)}
        >
          <span class="action-label">Play Hotseat</span>
          <span class="action-desc">Two players, one screen</span>
        </button>
        <button
          type="button"
          class="action-card primary"
          onClick={() => setAiOpen(true)}
        >
          <span class="action-label">Play vs AI</span>
          <span class="action-desc">Challenge the computer</span>
        </button>
        <button
          type="button"
          class="action-card primary"
          onClick={() => setOnlineOpen(true)}
        >
          <span class="action-label">Play Online</span>
          <span class="action-desc">Find an opponent</span>
        </button>
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
