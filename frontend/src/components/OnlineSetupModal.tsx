import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useOnlineGame } from "../api/online-game-context";
import type { PlayerColor } from "../board-logic";
import { sideOptions, timeOptions } from "../gameOptions";
import Button from "./ui/Button";
import ChipGrid from "./ui/ChipGrid";
import FormField from "./ui/FormField";
import Modal from "./ui/Modal";
import SegmentedControl from "./ui/SegmentedControl";

interface OnlineSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function OnlineSetupModal(props: OnlineSetupModalProps) {
  const navigate = useNavigate();
  const online = useOnlineGame();
  const [side, setSide] = createSignal("random");
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = async () => {
    const chosenSide: PlayerColor =
      side() === "random"
        ? Math.random() < 0.5
          ? "black"
          : "white"
        : (side() as PlayerColor);
    const { playerToken } = await online.createGame({
      creatorColor: chosenSide,
    });
    props.onOpenChange(false);
    navigate(`/game/online/${playerToken}`);
  };

  return (
    <Modal
      title="Play Online"
      open={props.open}
      onOpenChange={props.onOpenChange}
    >
      <FormField label="Play as">
        <SegmentedControl
          options={sideOptions}
          value={side()}
          onChange={setSide}
        />
      </FormField>

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
