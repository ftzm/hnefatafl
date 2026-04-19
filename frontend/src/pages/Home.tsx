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
          <span class="h" />
          <span class="d" />
          <span class="h" />
        </div>
        <p class="lede">King and kin against the siege.</p>
      </div>

      <HotseatSetupModal open={hotseatOpen()} onOpenChange={setHotseatOpen} />
      <AiSetupModal open={aiOpen()} onOpenChange={setAiOpen} />
      <OnlineSetupModal open={onlineOpen()} onOpenChange={setOnlineOpen} />

      <div class="entries">
        <a onClick={() => setAiOpen(true)}>
          <div class="body">
            <span class="lbl">Against AI</span>
            <span class="sub">
              Choose side and difficulty. Play untimed or against a clock.
            </span>
          </div>
          <div class="foot">
            Begin <span class="arr">&rarr;</span>
          </div>
        </a>
        <a onClick={() => setHotseatOpen(true)}>
          <div class="body">
            <span class="lbl">Hotseat</span>
            <span class="sub">
              Two players, one device. Name each and pass the screen.
            </span>
          </div>
          <div class="foot">
            Begin <span class="arr">&rarr;</span>
          </div>
        </a>
        <a onClick={() => setOnlineOpen(true)}>
          <div class="body">
            <span class="lbl">Online</span>
            <span class="sub">
              Invite a friend with a code, or join with one they've sent.
            </span>
          </div>
          <div class="foot">
            Begin <span class="arr">&rarr;</span>
          </div>
        </a>
      </div>

      <div class="recent">
        <Show when={mockInProgressGames.length > 0}>
          <section>
            <div class="sh">
              <span class="ey">In progress</span>
              <span class="ct">
                {mockInProgressGames.length}{" "}
                {mockInProgressGames.length === 1 ? "game" : "games"}
              </span>
            </div>
            <For each={mockInProgressGames}>
              {(game) => (
                <A href={`/game/${game.mode}/${game.id}`} class="ip-row">
                  <span class={`dot ${game.yourColor}`} />
                  <div class="who">
                    <span class="opp">
                      vs {game.opponent}
                      <Show when={game.opponentDetail}>
                        {" "}
                        <em>{game.opponentDetail}</em>
                      </Show>
                    </span>
                    <span class="meta">
                      {game.moveCount} moves &middot; {game.lastPlayed}
                    </span>
                  </div>
                  <span class={`badge${game.isYourTurn ? " turn" : ""}`}>
                    {game.status}
                  </span>
                  <span class="resume">Resume</span>
                </A>
              )}
            </For>
          </section>
        </Show>

        <Show when={mockCompletedGames.length > 0}>
          <section class="quiet">
            <div class="sh">
              <span class="ey">Recent</span>
              <span class="ct">last 30 days</span>
            </div>
            <For each={mockCompletedGames}>
              {(game) => (
                <A href={`/game/${game.mode}/${game.id}`} class="done-row">
                  <span class={`dot ${game.yourColor}`} />
                  <span class="opp">
                    vs {game.opponent}
                    <Show when={game.opponentDetail}>
                      {" "}
                      <em>{game.opponentDetail}</em>
                    </Show>
                  </span>
                  <span class={`res${game.isWin ? " win" : ""}`}>
                    {game.result}
                  </span>
                  <span class="date">{game.completedAt}</span>
                  <span class="chev">&rsaquo;</span>
                </A>
              )}
            </For>
          </section>
        </Show>
      </div>
    </div>
  );
}
