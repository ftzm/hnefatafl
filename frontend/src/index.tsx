/* @refresh reload */

import { Route, Router } from "@solidjs/router";
import { render } from "solid-js/web";
import AiGame from "./controllers/AiController";
import HotseatGame from "./controllers/HotseatController";
import OnlineGame from "./controllers/OnlineController";
import Layout from "./Layout";
import { MockHotseatApiProvider, MockAiGameProvider, MockOnlineGameProvider } from "./mocks/providers";
import Home from "./pages/Home";
import Settings from "./pages/Settings";
import "./tokens.css";
import "./styles.css";

render(
  () => (
    <MockHotseatApiProvider>
      <MockAiGameProvider>
        <MockOnlineGameProvider>
          <Router root={Layout}>
            <Route path="/" component={Home} />
            <Route path="/game/hotseat/:id" component={HotseatGame} />
            <Route path="/game/ai/:id" component={AiGame} />
            <Route path="/game/online/:id" component={OnlineGame} />
            <Route path="/settings" component={Settings} />
          </Router>
        </MockOnlineGameProvider>
      </MockAiGameProvider>
    </MockHotseatApiProvider>
  ),
  document.getElementById("root")!,
);
