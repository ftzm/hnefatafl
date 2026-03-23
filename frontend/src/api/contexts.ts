import { createServiceContext } from "../utils/create-service-context";
import type { HotseatApi } from "./hotseat-api";
import type { AiGameService } from "./ai-game-service";
import type { OnlineGameService } from "./online-game-service";

export const hotseatContext = createServiceContext<HotseatApi>("HotseatApi");
export const aiGameContext = createServiceContext<AiGameService>("AiGame");
export const onlineGameContext = createServiceContext<OnlineGameService>("OnlineGame");

export const useHotseatApi = hotseatContext.use;
export const useAiGame = aiGameContext.use;
export const useOnlineGame = onlineGameContext.use;
