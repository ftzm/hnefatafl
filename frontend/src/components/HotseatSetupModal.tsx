import { useNavigate } from "@solidjs/router";
import { createSignal, type Setter } from "solid-js";
import { useHotseatApi } from "../api/contexts";
import { timeOptions } from "../gameOptions";
import { useToasts } from "../toast-context";
import Modal from "./ui/Modal";
import OptionPicker from "./ui/OptionPicker";

interface HotseatSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

const DEFAULT_ATTACKER_NAME = "Olaf";
const DEFAULT_DEFENDER_NAME = "Inga";

export default function HotseatSetupModal(props: HotseatSetupModalProps) {
  const navigate = useNavigate();
  const hotseat = useHotseatApi();
  const { pushError } = useToasts();
  const [attackerName, setAttackerName] = createSignal(DEFAULT_ATTACKER_NAME);
  const [defenderName, setDefenderName] = createSignal(DEFAULT_DEFENDER_NAME);
  const [timeControl, setTimeControl] = createSignal("none");

  const startGame = async () => {
    try {
      const gameId = await hotseat.createGame();
      const black = attackerName().trim() || DEFAULT_ATTACKER_NAME;
      const white = defenderName().trim() || DEFAULT_DEFENDER_NAME;
      const query = new URLSearchParams({ black, white }).toString();
      props.onOpenChange(false);
      navigate(`/game/hotseat/${gameId}?${query}`);
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
      title={<em>Hotseat</em>}
      subtitle="Two players, one device. Name each and pass the screen."
    >
      <div class="modal-body">
        <span class="modal-label">Attackers</span>
        <div class="modal-value">
          <input
            class="modal-input"
            value={attackerName()}
            placeholder="Name"
            onInput={(e) => setAttackerName(e.currentTarget.value)}
          />
        </div>
        <span class="modal-label">Defenders</span>
        <div class="modal-value">
          <input
            class="modal-input"
            value={defenderName()}
            placeholder="Name"
            onInput={(e) => setDefenderName(e.currentTarget.value)}
          />
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
          Begin game
        </button>
      </div>
    </Modal>
  );
}
