/* @refresh reload */

import { Route, Router } from "@solidjs/router";
import { render } from "solid-js/web";
import {
  AiGameProvider,
  HotseatApiProvider,
  OnlineGameProvider,
} from "./api/providers";
import AiGame from "./controllers/AiController";
import HotseatGame from "./controllers/HotseatController";
import OnlineGame from "./controllers/OnlineController";
import Layout from "./Layout";
import Home from "./pages/Home";
import Settings from "./pages/Settings";
import { ToastProvider } from "./toast-context";
import "./tokens.css";
import "./styles.css";

render(
  () => (
    <ToastProvider>
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
    </ToastProvider>
  ),
  // biome-ignore lint/style/noNonNullAssertion: root element guaranteed by index.html
  document.getElementById("root")!,
);
