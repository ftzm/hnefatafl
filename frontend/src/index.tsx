/* @refresh reload */
import { render } from "solid-js/web";
import { Router, Route } from "@solidjs/router";
import Layout from "./Layout";
import Home from "./pages/Home";
import Settings from "./pages/Settings";
import HotseatGame from "./controllers/HotseatController";
import AiGame from "./controllers/AiController";
import OnlineGame from "./controllers/OnlineController";
import { HotseatApiProvider } from "./api/hotseat-context";
import { AiGameProvider } from "./api/ai-game-context";
import { OnlineGameProvider } from "./api/online-game-context";
import "./tokens.css";
import "./styles.css";

render(
  () => (
    <HotseatApiProvider>
      <AiGameProvider>
        <OnlineGameProvider>
          <Router root={Layout}>
            <Route path="/" component={Home} />
            <Route path="/game/hotseat/:id" component={HotseatGame} />
            <Route path="/game/ai/:id" component={AiGame} />
            <Route path="/game/online/:id" component={OnlineGame} />
            <Route path="/settings" component={Settings} />
          </Router>
        </OnlineGameProvider>
      </AiGameProvider>
    </HotseatApiProvider>
  ),
  document.getElementById("root")!,
);
