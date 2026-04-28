import { createSignal } from "solid-js";

const stored = localStorage.getItem("theme");
const initial = stored === "light";
if (initial) document.documentElement.classList.add("light");

const [light, setLight] = createSignal(initial);

export function isLight() {
  return light();
}

export function toggleTheme() {
  const next = !light();
  setLight(next);
  document.documentElement.classList.toggle("light", next);
  localStorage.setItem("theme", next ? "light" : "dark");
}
