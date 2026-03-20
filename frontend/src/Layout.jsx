import { A } from "@solidjs/router";
import Button from "./components/ui/Button.jsx";
import GearIcon from "./components/ui/icons/GearIcon.jsx";

export default function Layout(props) {
  return (
    <>
      <div class="top-bar">
        <A href="/" class="logo">HNEFATAFL</A>
        <nav class="nav-links">
          <A href="/" activeClass="active" end>Home</A>
        </nav>
        <div class="nav-right">
          <A href="/settings">
            <Button variant="ghost" aria-label="Settings">
              <GearIcon />
            </Button>
          </A>
        </div>
      </div>
      <div class="page-content">
        {props.children}
      </div>
    </>
  );
}
