import createClient from "openapi-fetch";
import type { paths } from "./generated/rest";

export const api = createClient<paths>({
  baseUrl: window.location.origin,
});
