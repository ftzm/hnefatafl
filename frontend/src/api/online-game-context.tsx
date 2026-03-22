import { createContext, useContext, type ParentComponent } from "solid-js";
import type { OnlineGameService } from "./online-game-service";
import { createMockOnlineGameService } from "./mock-online-game";

const OnlineGameContext = createContext<OnlineGameService>();

export const OnlineGameProvider: ParentComponent = (props) => {
  const service = createMockOnlineGameService();
  return (
    <OnlineGameContext.Provider value={service}>
      {props.children}
    </OnlineGameContext.Provider>
  );
};

export function useOnlineGame(): OnlineGameService {
  const ctx = useContext(OnlineGameContext);
  if (!ctx)
    throw new Error("useOnlineGame must be used within OnlineGameProvider");
  return ctx;
}
