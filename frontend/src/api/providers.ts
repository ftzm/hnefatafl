import { useToasts } from "../toast-context";
import { createAiGameService } from "./ai-game-service";
import { aiGameContext, hotseatContext, onlineGameContext } from "./contexts";
import { createHotseatApi } from "./hotseat-api";
import { createOnlineGameService } from "./online-game-service";

export const HotseatApiProvider =
  hotseatContext.createProvider(createHotseatApi);
export const AiGameProvider = aiGameContext.createProvider(() => {
  const { pushError } = useToasts();
  return createAiGameService({ onError: pushError });
});
export const OnlineGameProvider = onlineGameContext.createProvider(() => {
  const { pushError } = useToasts();
  return createOnlineGameService({ onError: pushError });
});
