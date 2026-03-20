/* @refresh reload */
import { render } from "solid-js/web";
import { Router, Route } from "@solidjs/router";
import Layout from "./Layout.jsx";
import Home from "./pages/Home.jsx";
import Game from "./pages/Game.jsx";
import Settings from "./pages/Settings.jsx";
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
  document.getElementById("root"),
);
