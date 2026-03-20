import { A } from "@solidjs/router";

export default function GameCard(props) {
  const game = () => props.game;

  return (
    <A href={`/game/${game().id}`} class="game-card">
      <div class="game-card-left">
        <span class={`role-chip ${game().yourColor}`} />
        <div class="game-card-info">
          <span class="game-card-opponent">vs {game().opponent}</span>
          <span class="game-card-meta">
            {game().moveCount} moves · {game().completedAt || game().lastPlayed}
          </span>
        </div>
      </div>
      <span class="game-card-status">
        {game().result || game().status}
      </span>
    </A>
  );
}
