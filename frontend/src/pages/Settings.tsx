import { createSignal } from "solid-js";
import Card from "../components/ui/Card";
import Toggle from "../components/ui/Toggle";

export default function Settings() {
  const [soundEnabled, setSoundEnabled] = createSignal(true);

  return (
    <div class="settings-page">
      <h1 class="settings-title">Settings</h1>

      <Card class="settings-group">
        <h2 class="settings-group-title">Appearance</h2>
        <div class="setting-row">
          <span class="setting-name">Theme</span>
          <span class="setting-value-muted">Dark</span>
        </div>
      </Card>

      <Card class="settings-group">
        <h2 class="settings-group-title">Sound</h2>
        <div class="setting-row">
          <Toggle
            label="Sound effects"
            checked={soundEnabled()}
            onChange={setSoundEnabled}
          />
        </div>
      </Card>

      <Card class="settings-group">
        <h2 class="settings-group-title">Game Defaults</h2>
        <div class="setting-row">
          <span class="setting-name">Default time control</span>
          <span class="setting-value-muted">None</span>
        </div>
      </Card>
    </div>
  );
}
