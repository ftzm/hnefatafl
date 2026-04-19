import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useOnlineGame } from "../api/contexts";
import type { PlayerColor } from "../board-logic";
import { sideOptions } from "../gameOptions";
import ChipGrid from "./ui/ChipGrid";
import Modal from "./ui/Modal";

interface OnlineSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function OnlineSetupModal(props: OnlineSetupModalProps) {
  const navigate = useNavigate();
  const online = useOnlineGame();
  const [side, setSide] = createSignal("black");

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
      open={props.open}
      onOpenChange={props.onOpenChange}
      eyebrow="New game"
      title={<em>Online</em>}
      subtitle="Send this code to a friend, or join one they've sent you."
    >
      <div class="modal-bd">
        <div class="modal-bd-inner">
          <div class="modal-bd-labels">
            <span class="modal-field-k">Your code</span>
            <span class="modal-field-k">Side</span>
            <span class="modal-field-k">Join code</span>
          </div>
          <div class="modal-bd-values">
            <div class="modal-field-v">
              <div class="modal-code-row">
                <span class="modal-code">K4N-2BR</span>
                <span class="modal-copy">Copy</span>
              </div>
            </div>
            <div class="modal-field-v">
              <ChipGrid
                options={sideOptions}
                value={side()}
                onChange={setSide}
              />
            </div>
            <div class="modal-field-v">
              <input
                class="modal-input"
                placeholder="6-letter invite from a friend"
              />
            </div>
          </div>
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
