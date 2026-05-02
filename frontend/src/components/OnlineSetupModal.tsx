import { useNavigate } from "@solidjs/router";
import { createSignal, Match, type Setter, Switch } from "solid-js";
import { useOnlineGame } from "../api/contexts";
import type { PlayerColor } from "../board-logic";
import { sideOptions, timeOptions } from "../gameOptions";
import { useToasts } from "../toast-context";
import Modal from "./ui/Modal";
import OptionPicker from "./ui/OptionPicker";

interface OnlineSetupModalProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
}

type Step = "setup" | "share";

const COPIED_FEEDBACK_MS = 1500;

export default function OnlineSetupModal(props: OnlineSetupModalProps) {
  const navigate = useNavigate();
  const online = useOnlineGame();
  const { pushError } = useToasts();
  const [side, setSide] = createSignal("black");
  const [timeControl, setTimeControl] = createSignal("none");
  const [step, setStep] = createSignal<Step>("setup");
  const [playerToken, setPlayerToken] = createSignal<string | undefined>();
  const [inviteToken, setInviteToken] = createSignal<string | undefined>();
  const [copied, setCopied] = createSignal(false);

  const inviteUrl = () => {
    const token = inviteToken();
    if (!token) return "";
    return `${window.location.origin}/game/online/${token}`;
  };

  const resetModal = () => {
    setStep("setup");
    setPlayerToken(undefined);
    setInviteToken(undefined);
    setCopied(false);
  };

  const handleOpenChange = (open: boolean) => {
    if (!open) resetModal();
    props.onOpenChange(open);
  };

  const startGame = async () => {
    const chosenSide: PlayerColor =
      side() === "random"
        ? Math.random() < 0.5
          ? "black"
          : "white"
        : (side() as PlayerColor);
    try {
      const tokens = await online.createGame({ creatorColor: chosenSide });
      setPlayerToken(tokens.playerToken);
      setInviteToken(tokens.inviteToken);
      setStep("share");
    } catch {
      pushError({
        code: "connection_error",
        message: "Server unreachable",
        fatal: false,
      });
    }
  };

  const copyInvite = async () => {
    const url = inviteUrl();
    if (!url) return;
    try {
      await navigator.clipboard.writeText(url);
      setCopied(true);
      setTimeout(() => setCopied(false), COPIED_FEEDBACK_MS);
    } catch (err) {
      console.error(
        "clipboard.writeText failed",
        {
          hasFocus: document.hasFocus(),
          activeElement: document.activeElement,
        },
        err,
      );
      pushError({
        code: "clipboard_error",
        message: "Couldn't copy to clipboard",
        fatal: false,
      });
    }
  };

  const continueToGame = () => {
    const token = playerToken();
    if (!token) return;
    handleOpenChange(false);
    navigate(`/game/online/${token}`);
  };

  return (
    <Modal
      open={props.open}
      onOpenChange={handleOpenChange}
      eyebrow="New game"
      title={
        <Switch>
          <Match when={step() === "setup"}>
            <em>Online</em>
          </Match>
          <Match when={step() === "share"}>Game created</Match>
        </Switch>
      }
      subtitle={
        step() === "setup"
          ? "Play against a friend online."
          : "Share the link with your opponent."
      }
    >
      <Switch>
        <Match when={step() === "setup"}>
          <div class="modal-body">
            <span class="modal-label">Side</span>
            <div class="modal-value">
              <OptionPicker
                options={sideOptions}
                value={side()}
                onChange={setSide}
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
              onClick={() => handleOpenChange(false)}
            >
              Cancel
            </button>
            <button type="button" class="modal-btn" onClick={startGame}>
              Create game
            </button>
          </div>
        </Match>
        <Match when={step() === "share"}>
          <div class="modal-body">
            <div class="invite-url">{inviteUrl()}</div>
          </div>
          <div class="modal-actions">
            <button type="button" class="modal-btn" onClick={copyInvite}>
              {copied() ? "Copied" : "Copy link"}
            </button>
            <button type="button" class="modal-btn" onClick={continueToGame}>
              Continue to game
            </button>
          </div>
        </Match>
      </Switch>
    </Modal>
  );
}
