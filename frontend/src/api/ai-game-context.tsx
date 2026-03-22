import { createContext, useContext, type ParentComponent } from "solid-js";
import type { AiGameService } from "./ai-game-service";
import { createMockAiGameService } from "./mock-ai-game";

const AiGameContext = createContext<AiGameService>();

export const AiGameProvider: ParentComponent = (props) => {
  const service = createMockAiGameService();
  return (
    <AiGameContext.Provider value={service}>
      {props.children}
    </AiGameContext.Provider>
  );
};

export function useAiGame(): AiGameService {
  const ctx = useContext(AiGameContext);
  if (!ctx) throw new Error("useAiGame must be used within AiGameProvider");
  return ctx;
}
