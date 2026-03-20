import { createSignal, type Setter } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Modal from "./ui/Modal";
import Button from "./ui/Button";
import SegmentedControl from "./ui/SegmentedControl";
import ChipGrid from "./ui/ChipGrid";
import FormField from "./ui/FormField";
import { handleNewGame } from "../state";
import { sideOptions, timeOptions } from "../gameOptions";

interface OnlineSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function OnlineSetupModal(props: OnlineSetupModalProps) {
  const navigate = useNavigate();
  const [side, setSide] = createSignal("random");
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = () => {
    const id = crypto.randomUUID();
    const chosenSide = side() === "random"
      ? (Math.random() < 0.5 ? "black" : "white")
      : side();
    const players = chosenSide === "black"
      ? { black: "You", white: "Opponent" }
      : { black: "Opponent", white: "You" };
    handleNewGame({ id, mode: "online", players });
    props.onOpenChange(false);
    navigate(`/game/${id}`);
  };

  return (
    <Modal title="Play Online" open={props.open} onOpenChange={props.onOpenChange}>
      <FormField label="Play as">
        <SegmentedControl
          options={sideOptions}
          value={side()}
          onChange={setSide}
        />
      </FormField>

      <FormField label="Time Control">
        <ChipGrid options={timeOptions} value={timeControl()} onChange={setTimeControl} />
      </FormField>

      <Button onClick={startGame} class="setup-start-btn">Start Game</Button>
    </Modal>
  );
}
