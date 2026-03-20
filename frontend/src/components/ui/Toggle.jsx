import { Switch } from "@kobalte/core/switch";

export default function Toggle(props) {
  return (
    <Switch class="toggle" checked={props.checked} onChange={props.onChange}>
      <Switch.Input />
      <Switch.Control class="toggle-control">
        <Switch.Thumb class="toggle-thumb" />
      </Switch.Control>
      <Switch.Label class="toggle-label">{props.label}</Switch.Label>
    </Switch>
  );
}
