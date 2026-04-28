import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useOnlineGame } from "../api/contexts";
import type { PlayerColor } from "../board-logic";
import { sideOptions, timeOptions } from "../gameOptions";
import { useToasts } from "../toast-context";
import OptionPicker from "./ui/OptionPicker";
import Modal from "./ui/Modal";

interface OnlineSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function OnlineSetupModal(props: OnlineSetupModalProps) {
  const navigate = useNavigate();
  const online = useOnlineGame();
  const { pushError } = useToasts();
  const [side, setSide] = createSignal("black");
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = async () => {
    const chosenSide: PlayerColor =
      side() === "random"
        ? Math.random() < 0.5
          ? "black"
          : "white"
        : (side() as PlayerColor);
    try {
      const { playerToken } = await online.createGame({
        creatorColor: chosenSide,
      });
      props.onOpenChange(false);
      navigate(`/game/online/${playerToken}`);
    } catch {
      pushError({
        code: "connection_error",
        message: "Server unreachable",
        fatal: false,
      });
    }
  };

  return (
    <Modal
      open={props.open}
      onOpenChange={props.onOpenChange}
      eyebrow="New game"
      title={<em>Online</em>}
      subtitle="Play against a friend online."
    >
      <div class="modal-body">
        <span class="modal-label">Side</span>
        <div class="modal-value">
          <OptionPicker options={sideOptions} value={side()} onChange={setSide} />
        </div>
        <span class="modal-label">Time</span>
        <div class="modal-value">
          <OptionPicker
            options={timeOptions}
            value={timeControl()}
            onChange={setTimeControl}
          />
        </div>
      </div>
      <div class="modal-actions">
        <button
          type="button"
          class="modal-btn"
          onClick={() => props.onOpenChange(false)}
        >
          Cancel
        </button>
        <button type="button" class="modal-btn" onClick={startGame}>
          Open invite
        </button>
      </div>
    </Modal>
  );
}
