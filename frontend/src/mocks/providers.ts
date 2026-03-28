import {
  aiGameContext,
  hotseatContext,
  onlineGameContext,
} from "../api/contexts";
import { createMockAiGameService } from "./mock-ai-game";
import { createMockHotseatApi } from "./mock-hotseat";
import { createMockOnlineGameService } from "./mock-online-game";

export const MockHotseatApiProvider =
  hotseatContext.createProvider(createMockHotseatApi);
export const MockAiGameProvider = aiGameContext.createProvider(
  createMockAiGameService,
);
export const MockOnlineGameProvider = onlineGameContext.createProvider(
  createMockOnlineGameService,
);
