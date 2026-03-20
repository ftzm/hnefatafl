import { Switch } from "@kobalte/core/switch";

interface ToggleProps {
  label: string;
  checked: boolean;
  onChange: (checked: boolean) => void;
}

export default function Toggle(props: ToggleProps) {
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
