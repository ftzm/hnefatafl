import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useHotseatApi } from "../api/contexts";
import { timeOptions } from "../gameOptions";
import { useToasts } from "../toast-context";
import ChipGrid from "./ui/ChipGrid";
import Modal from "./ui/Modal";

interface HotseatSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

export default function HotseatSetupModal(props: HotseatSetupModalProps) {
  const navigate = useNavigate();
  const hotseat = useHotseatApi();
  const { pushError } = useToasts();
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = async () => {
    try {
      const gameId = await hotseat.createGame();
      props.onOpenChange(false);
      navigate(`/game/hotseat/${gameId}`);
    } catch {
      pushError({ code: "connection_error", message: "Server unreachable", fatal: false });
    }
  };

  return (
    <Modal
      open={props.open}
      onOpenChange={props.onOpenChange}
      eyebrow="New game"
      title={<em>Hotseat</em>}
      subtitle="Two players, one device. Name each and pass the screen."
    >
      <div class="modal-bd">
        <div class="modal-bd-inner">
          <div class="modal-bd-labels">
            <span class="modal-field-k">Attackers</span>
            <span class="modal-field-k">Defenders</span>
            <span class="modal-field-k">Time</span>
          </div>
          <div class="modal-bd-values">
            <div class="modal-field-v">
              <input class="modal-input" value="Olaf" placeholder="Name" />
            </div>
            <div class="modal-field-v">
              <input class="modal-input" value="Inga" placeholder="Name" />
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
