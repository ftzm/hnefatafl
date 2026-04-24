import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useAiGame } from "../api/contexts";
import type { PlayerColor } from "../board-logic";
import { sideOptions, timeOptions } from "../gameOptions";
import { useToasts } from "../toast-context";
import ChipGrid from "./ui/ChipGrid";
import Modal from "./ui/Modal";

interface AiSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function AiSetupModal(props: AiSetupModalProps) {
  const navigate = useNavigate();
  const ai = useAiGame();
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
      const { token } = await ai.createGame({ playerColor: chosenSide });
      props.onOpenChange(false);
      navigate(`/game/ai/${token}`);
    } catch {
      pushError({ code: "connection_error", message: "Server unreachable", fatal: false });
    }
  };

  return (
    <Modal
      open={props.open}
      onOpenChange={props.onOpenChange}
      eyebrow="New game"
      title={
        <>
          Against <em>AI</em>
        </>
      }
      subtitle="Pick your side and time control."
    >
      <div class="modal-bd">
        <div class="modal-bd-inner">
          <div class="modal-bd-labels">
            <span class="modal-field-k">Side</span>
            <span class="modal-field-k">Time</span>
          </div>
          <div class="modal-bd-values">
            <div class="modal-field-v">
              <ChipGrid
                options={sideOptions}
                value={side()}
                onChange={setSide}
              />
            </div>
            <div class="modal-field-v">
              <ChipGrid
                options={timeOptions}
                value={timeControl()}
                onChange={setTimeControl}
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
          Begin game
        </button>
      </div>
    </Modal>
  );
}
