# Hnefatafl Frontend — SolidJS TypeScript UI

SolidJS single-page app with Vite. Currently runs entirely on mock backends (no real API integration yet).

## Commands

```
npm install            # install dependencies
npm run dev            # dev server on http://localhost:3000
npm run build          # production build
npm run preview        # preview production build
npm run typecheck      # tsc --noEmit
npm run lint           # biome check src
npm run lint:fix       # biome check --write src
npm run test           # vitest (unit tests)
npm run test:watch     # vitest in watch mode
npm run test:e2e       # playwright (e2e tests, starts dev server automatically)
```

## Project Layout

```
src/
  index.tsx              Entry point: router + provider hierarchy
  Layout.tsx             Top nav bar wrapper
  game-context.tsx       Central game state (createStore) + all game actions
  board-logic.ts         Board types, move application, legal move computation
  gameOptions.ts         Game configuration options
  tokens.css             Design system CSS variables (colors, spacing, typography, radius)
  styles.css             Global + component styles
  pages/
    Home.tsx             Game mode selection + setup modals
    Settings.tsx         User preferences
  components/
    Board.tsx            11x11 SVG board with drag-and-drop, move highlighting, animations
    GameLayout.tsx       Main game container (responsive two-pane: board + sidebar)
    Controls.tsx         Move history list + navigation toolbar + action buttons
    MoveHistory.tsx      Move pairs display with click-to-jump
    PlayerBar.tsx        Player name, color indicator, captured piece count
    GameStatus.tsx       Current turn or game result display
    Chat.tsx             Chat message list + input
    *SetupModal.tsx      Game setup dialogs (Hotseat, AI, Online)
    ui/                  Reusable primitives: Button, Modal, BottomSheet, Card, Panel,
                         SegmentedControl, ChipGrid, FormField, Toggle, Toolbar, icons/
  controllers/
    HotseatController.tsx   Local game: calls HotseatApi after each move
    AiController.tsx        AI game: listens to AiGameService events, applies AI moves
    OnlineController.tsx    Multiplayer: WebSocket events, draw/undo negotiation
  api/
    types.ts             Service interfaces: HotseatApi, AiGameService, OnlineGameService
    contexts.ts          Service context definitions (createServiceContext pattern)
    hotseat-api.ts       Real hotseat API implementation
    ai-game-service.ts   Real AI game service
    online-game-service.ts Real online game service
  mocks/
    providers.ts         Mock provider wrappers (currently used as the active providers in index.tsx)
    mock-hotseat.ts      Mock hotseat: computes moves client-side
    mock-ai-game.ts      Mock AI: random moves with delay
    mock-online-game.ts  Mock multiplayer
    mock-game-logic.ts   Client-side game rules (move gen, capture, win detection)
    mock-data.ts         Sample game data for home page
  utils/
    create-service-context.tsx   Generic context + provider factory
    async-lock.ts        FIFO mutex for sequential async operations (used in history navigation)
  test-setup.ts          Mocks for jsdom: ResizeObserver, Element.animate, scrollIntoView
```

## Architecture

### State Management

`game-context.tsx` provides `GameProvider` / `useGame()`. State is a SolidJS `createStore`:

```typescript
interface GameState {
  boardRep: BoardRep;          // { black: Set<number>, white: Set<number>, king: number }
  moveHistory: Move[];         // { from, to, captures? }[]
  currentPlayer: PlayerColor;  // "black" | "white"
  moves: MovesMap;             // Record<number, number[][]> — legal moves per square
  capturedPieces: { black: number; white: number };
  gameOver: GameOverState | null;
  playerColor: PlayerColor | null;
  players: { black: string; white: string };
  historyCursor: number;       // for move history navigation
}
```

Key methods: `applyMove()`, `applyExternalMove()` (with animation), `undoLastMove()`, `viewPrev()`/`viewNext()`/`jumpToMove()`, `reconcile()`.

### Service Pattern

Services use dependency injection via `createServiceContext<T>()`:
1. Define interface (e.g. `HotseatApi`)
2. Create context + hook via `createServiceContext`
3. Provide real or mock implementation via provider component
4. Controllers consume via hook (e.g. `useHotseatApi()`)

Currently `index.tsx` wraps routes in `MockHotseatApiProvider → MockAiGameProvider → MockOnlineGameProvider`. To integrate real backend: swap mock providers for real ones.

### Controller Pattern

Each game mode has a controller component that:
- Wraps content in `GameProvider`
- Connects to the appropriate service on mount
- Listens to service events via `createEffect(on(service.events, ...))`
- Translates service events into game context actions
- Passes callbacks (`onMove`, `onUndo`, `onResign`) to `GameLayout`

### Board Representation

Same 0-120 index space as the C engine (11x11 grid). `board-logic.ts` contains:
- `BoardRep` — `{ black: Set<number>, white: Set<number>, king: number }`
- `Move` — `{ from: number, to: number, captures?: number[] }`
- `MovesMap` — `Record<number, number[][]>` (legal destinations grouped by square)
- `indexToAlgebraic()` — converts index to chess-style notation (a1-k11)
- `startBoard` — initial piece placement (24 black, 12 white, 1 king)

### Routing

`@solidjs/router` with routes:
- `/` — Home
- `/game/hotseat/:id` — Hotseat game
- `/game/ai/:id` — AI game
- `/game/online/:id` — Online game
- `/settings` — Settings

### API Communication

Event-driven pattern with discriminated unions:

```typescript
type AiGameEvent =
  | { type: "initialState"; ... }
  | { type: "moveMade"; move; boardRep; currentPlayer; moves }
  | { type: "gameOver"; winner; reason }

type OnlineGameEvent = AiGameEvent
  | { type: "opponentJoined" | "opponentLeft" }
  | { type: "drawOffer" | "undoRequest"; ... }
  | { type: "chat"; message }
```

## Styling

CSS-only (no CSS-in-JS). Design tokens in `tokens.css`:
- Color palettes: gray (9 shades), warm (pieces/backgrounds), wood (board)
- Spacing scale: `--space-1` (0.0625rem) to `--space-24` (1.5rem)
- Typography: `--text-xs` to `--text-lg`
- Radius: `--radius-sm/md/lg/full`

Component styles in `styles.css`. BEM-ish naming (`.component-name__element`). Mobile-first responsive with `.mobile-only`/`.desktop-only` classes. Data attributes for board squares: `[data-index]`.

## Code Conventions

- **Formatting/Linting**: Biome (2-space indent, double quotes, 80 columns, trailing commas)
- **Components**: PascalCase filenames + exports. Props interfaces named `[Component]Props`
- **Reactivity**: SolidJS primitives — `createSignal`, `createMemo`, `createStore`, `createEffect`
- **Events**: Discriminated unions with `type` field
- **Test selectors**: `[data-index]` attributes on board squares, standard text content matching elsewhere
- **UI library**: Kobalte for headless components (Dialog, Button)

## Testing

- **Unit**: Vitest + `@solidjs/testing-library`. Files: `*.test.ts(x)`. Config in `vitest.config.ts`
- **E2E**: Playwright. Tests in `e2e/`. Config in `playwright.config.ts`. Auto-starts dev server

## Biome Overrides

Board.tsx has a11y rules disabled (`useKeyWithClickEvents`, `noSvgWithoutTitle`, `noStaticElementInteractions`) since the SVG board interaction model requires pointer events on non-semantic elements.
