/** Test hooks exposed by mock services for Playwright and console use. */
interface Window {
  __simulateTimeout?: (delayMs?: number) => void;
}
