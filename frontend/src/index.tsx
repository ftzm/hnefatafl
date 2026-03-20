/* @refresh reload */
import { render } from "solid-js/web";
import { Router, Route } from "@solidjs/router";
import Layout from "./Layout";
import Home from "./pages/Home";
import Game from "./pages/Game";
import Settings from "./pages/Settings";
import "./tokens.css";
import "./styles.css";

render(
  () => (
    <Router root={Layout}>
      <Route path="/" component={Home} />
      <Route path="/game/:id" component={Game} />
      <Route path="/settings" component={Settings} />
    </Router>
  ),
  document.getElementById("root")!,
);
