import { createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Modal from "./ui/Modal.jsx";
import Button from "./ui/Button.jsx";
import ChipGrid from "./ui/ChipGrid.jsx";
import FormField from "./ui/FormField.jsx";
import { handleNewGame } from "../state.js";
import { timeOptions } from "../gameOptions.js";

export default function HotseatSetupModal(props) {
  const navigate = useNavigate();
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = () => {
    const id = crypto.randomUUID();
    handleNewGame({ id, mode: "hotseat" });
    props.onOpenChange(false);
    navigate(`/game/${id}`);
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
