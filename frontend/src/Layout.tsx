import { A } from "@solidjs/router";
import { createSignal, For, type ParentProps } from "solid-js";
import GearIcon from "./components/ui/icons/GearIcon";
import MoonIcon from "./components/ui/icons/MoonIcon";
import SunIcon from "./components/ui/icons/SunIcon";
import { useToasts } from "./toast-context";

export default function Layout(props: ParentProps) {
  const [dark, setDark] = createSignal(false);
  const { toasts, dismiss } = useToasts();

  const toggleTheme = () => {
    setDark(!dark());
    document.documentElement.classList.toggle("dark", dark());
  };

  return (
    <>
      <div class="top-bar">
        <A href="/" class="wm">
          Hnefatafl
        </A>
        <div class="tools">
          <button
            type="button"
            class="icon-btn"
            onClick={toggleTheme}
            aria-label="Toggle dark mode"
          >
            {dark() ? <SunIcon /> : <MoonIcon />}
          </button>
          <A href="/settings" class="icon-btn" aria-label="Settings">
            <GearIcon />
          </A>
        </div>
      </div>
      <div class="page-content">{props.children}</div>
      <div class="toast-stack" data-kb-top-layer>
        <For each={toasts()}>
          {(toast) => (
            <div class="error-toast">
              <span class="error-toast__message">{toast.error.message}</span>
              <button
                type="button"
                class="error-toast__close"
                onClick={() => dismiss(toast.id)}
              >
                &times;
              </button>
            </div>
          )}
        </For>
      </div>
    </>
  );
}
