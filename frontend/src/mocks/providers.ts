import { hotseatContext, aiGameContext, onlineGameContext } from "../api/contexts";
import { createMockHotseatApi } from "./mock-hotseat";
import { createMockAiGameService } from "./mock-ai-game";
import { createMockOnlineGameService } from "./mock-online-game";

export const MockHotseatApiProvider = hotseatContext.createProvider(createMockHotseatApi);
export const MockAiGameProvider = aiGameContext.createProvider(createMockAiGameService);
export const MockOnlineGameProvider = onlineGameContext.createProvider(createMockOnlineGameService);
