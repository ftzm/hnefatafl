import { A } from "@solidjs/router";
import type { ParentProps } from "solid-js";

export default function Layout(props: ParentProps) {
  return (
    <>
      <div class="top-bar">
        <A href="/" class="wm">
          Hnefatafl
        </A>
        <nav>
          <A href="/" activeClass="active" end>
            Home
          </A>
          <a>Play</a>
          <a>Learn</a>
        </nav>
        <div class="tools">
          <A href="/settings">Settings</A>
        </div>
      </div>
      <div class="page-content">{props.children}</div>
    </>
  );
}
