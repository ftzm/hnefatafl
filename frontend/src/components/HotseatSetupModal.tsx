import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useHotseatApi } from "../api/contexts";
import { timeOptions } from "../gameOptions";
import Button from "./ui/Button";
import ChipGrid from "./ui/ChipGrid";
import FormField from "./ui/FormField";
import Modal from "./ui/Modal";

interface HotseatSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function HotseatSetupModal(props: HotseatSetupModalProps) {
  const navigate = useNavigate();
  const hotseat = useHotseatApi();
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = async () => {
    const gameId = await hotseat.createGame();
    props.onOpenChange(false);
    navigate(`/game/hotseat/${gameId}`);
  };

  return (
    <Modal
      title="Play Hotseat"
      open={props.open}
      onOpenChange={props.onOpenChange}
    >
      <FormField label="Time Control">
        <ChipGrid
          options={timeOptions}
          value={timeControl()}
          onChange={setTimeControl}
        />
      </FormField>

      <Button onClick={startGame} class="setup-start-btn">
        Start Game
      </Button>
    </Modal>
  );
}
