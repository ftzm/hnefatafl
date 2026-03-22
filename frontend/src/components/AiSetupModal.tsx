import { createSignal, type Setter } from "solid-js";
import { useNavigate } from "@solidjs/router";
import Modal from "./ui/Modal";
import Button from "./ui/Button";
import SegmentedControl from "./ui/SegmentedControl";
import ChipGrid from "./ui/ChipGrid";
import FormField from "./ui/FormField";
import { useAiGame } from "../api/ai-game-context";
import { sideOptions, timeOptions } from "../gameOptions";
import type { PlayerColor } from "../board-logic";

interface AiSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function AiSetupModal(props: AiSetupModalProps) {
  const navigate = useNavigate();
  const ai = useAiGame();
  const [side, setSide] = createSignal("random");
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = async () => {
    const chosenSide: PlayerColor = side() === "random"
      ? (Math.random() < 0.5 ? "black" : "white")
      : side() as PlayerColor;
    const { token } = await ai.createGame({ playerColor: chosenSide });
    props.onOpenChange(false);
    navigate(`/game/ai/${token}`);
  };

  return (
    <Modal title="Play vs AI" open={props.open} onOpenChange={props.onOpenChange}>
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
