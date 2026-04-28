import { A } from "@solidjs/router";
import { createSignal, For, Show } from "solid-js";
import AiSetupModal from "../components/AiSetupModal";
import HotseatSetupModal from "../components/HotseatSetupModal";
import OnlineSetupModal from "../components/OnlineSetupModal";
import { mockCompletedGames, mockInProgressGames } from "../mocks/mock-data";

export default function Home() {
  const [hotseatOpen, setHotseatOpen] = createSignal(false);
  const [aiOpen, setAiOpen] = createSignal(false);
  const [onlineOpen, setOnlineOpen] = createSignal(false);

  return (
    <div class="home">
      <div class="hero">
        <span class="eyebrow">The Viking board game of strategy</span>
        <h1>
          Hnefa<em>tafl</em>
        </h1>
        <div class="dot-rule">
          <span class="line" />
          <span class="dot" />
          <span class="line" />
        </div>
        <p class="lede">King and kin against the siege.</p>
      </div>

      <HotseatSetupModal open={hotseatOpen()} onOpenChange={setHotseatOpen} />
      <AiSetupModal open={aiOpen()} onOpenChange={setAiOpen} />
      <OnlineSetupModal open={onlineOpen()} onOpenChange={setOnlineOpen} />

      <div class="entries">
        <a onClick={() => setAiOpen(true)}>
          <div class="body">
            <span class="title">Against AI</span>
            <span class="description">
              Pick your side and time control, then take on the engine.
            </span>
          </div>
          <div class="footer">
            Play <span class="arrow">&rsaquo;</span>
          </div>
        </a>
        <a onClick={() => setHotseatOpen(true)}>
          <div class="body">
            <span class="title">Hotseat</span>
            <span class="description">
              Two players, one device. Name each and pass the screen.
            </span>
          </div>
          <div class="footer">
            Play <span class="arrow">&rsaquo;</span>
          </div>
        </a>
        <a onClick={() => setOnlineOpen(true)}>
          <div class="body">
            <span class="title">Online</span>
            <span class="description">
              Challenge a friend to a match over the internet.
            </span>
          </div>
          <div class="footer">
            Play <span class="arrow">&rsaquo;</span>
          </div>
        </a>
      </div>

      <div class="recent">
        <Show when={mockInProgressGames.length > 0}>
          <section>
            <div class="section-header">
              <span class="eyebrow">In progress</span>
              <span class="detail">
                {mockInProgressGames.length}{" "}
                {mockInProgressGames.length === 1 ? "game" : "games"}
              </span>
            </div>
            <For each={mockInProgressGames}>
              {(game) => (
                <A
                  href={`/game/${game.mode}/${game.id}`}
                  class="game-row ip-row"
                >
                  <span class={`dot ${game.yourColor}`} />
                  <span class="opponent">
                    vs {game.opponent}
                    <Show when={game.opponentDetail}>
                      {" "}
                      <em>{game.opponentDetail}</em>
                    </Show>
                  </span>
                  <span class="meta">{game.lastPlayed}</span>
                  <span class={`tag${game.isYourTurn ? " turn" : ""}`}>
                    {game.status}
                  </span>
                </A>
              )}
            </For>
          </section>
        </Show>

        <Show when={mockCompletedGames.length > 0}>
          <section class="quiet">
            <div class="section-header">
              <span class="eyebrow">Recent</span>
              <span class="detail">last 30 days</span>
            </div>
            <For each={mockCompletedGames}>
              {(game) => (
                <A
                  href={`/game/${game.mode}/${game.id}`}
                  class="game-row done-row"
                >
                  <span class={`dot ${game.yourColor}`} />
                  <span class="opponent">
                    vs {game.opponent}
                    <Show when={game.opponentDetail}>
                      {" "}
                      <em>{game.opponentDetail}</em>
                    </Show>
                  </span>
                  <span class="meta">{game.completedAt}</span>
                  <span class={`tag${game.isWin ? " win" : ""}`}>
                    {game.result}
                  </span>
                </A>
              )}
            </For>
          </section>
        </Show>
      </div>
    </div>
  );
}
