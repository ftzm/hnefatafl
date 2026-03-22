import { createContext, useContext, type ParentComponent } from "solid-js";
import type { HotseatApi } from "./hotseat-api";
import { createMockHotseatApi } from "./mock-hotseat";

const HotseatApiContext = createContext<HotseatApi>();

export const HotseatApiProvider: ParentComponent = (props) => {
  const api = createMockHotseatApi();
  return (
    <HotseatApiContext.Provider value={api}>
      {props.children}
    </HotseatApiContext.Provider>
  );
};

export function useHotseatApi(): HotseatApi {
  const ctx = useContext(HotseatApiContext);
  if (!ctx)
    throw new Error("useHotseatApi must be used within HotseatApiProvider");
  return ctx;
}
