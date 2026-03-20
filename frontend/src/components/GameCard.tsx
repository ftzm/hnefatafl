import { A } from "@solidjs/router";
import type { InProgressGame, CompletedGame } from "../mock-data";

interface GameCardProps {
  game: InProgressGame | CompletedGame;
}

export default function GameCard(props: GameCardProps) {
  const game = () => props.game;

  return (
    <A href={`/game/${game().id}`} class="game-card">
      <div class="game-card-left">
        <span class={`role-chip ${game().yourColor}`} />
        <div class="game-card-info">
          <span class="game-card-opponent">vs {game().opponent}</span>
          <span class="game-card-meta">
            {game().moveCount} moves · {"completedAt" in game() ? (game() as CompletedGame).completedAt : (game() as InProgressGame).lastPlayed}
          </span>
        </div>
      </div>
      <span class="game-card-status">
        {"result" in game() ? (game() as CompletedGame).result : (game() as InProgressGame).status}
      </span>
    </A>
  );
}
