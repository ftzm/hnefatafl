import { createSignal, type Setter } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Modal from "./ui/Modal";
import Button from "./ui/Button";
import ChipGrid from "./ui/ChipGrid";
import FormField from "./ui/FormField";
import { timeOptions } from "../gameOptions";

interface HotseatSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function HotseatSetupModal(props: HotseatSetupModalProps) {
  const navigate = useNavigate();
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = () => {
    const id = crypto.randomUUID();
    props.onOpenChange(false);
    navigate(`/game/hotseat/${id}`);
  };

  return (
    <Modal title="Play Hotseat" open={props.open} onOpenChange={props.onOpenChange}>
      <FormField label="Time Control">
        <ChipGrid options={timeOptions} value={timeControl()} onChange={setTimeControl} />
      </FormField>

      <Button onClick={startGame} class="setup-start-btn">Start Game</Button>
    </Modal>
  );
}
