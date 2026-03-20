import { Dialog } from "@kobalte/core/dialog";
import CloseIcon from "./icons/CloseIcon.jsx";

export default function Modal(props) {
  return (
    <Dialog open={props.open} onOpenChange={props.onOpenChange}>
      <Dialog.Portal>
        <Dialog.Overlay class="modal-overlay" />
        <Dialog.Content class="modal-content">
          <Dialog.Title class="modal-title">{props.title}</Dialog.Title>
          {props.children}
          <Dialog.CloseButton class="btn btn-ghost modal-close" aria-label="Close">
            <CloseIcon />
          </Dialog.CloseButton>
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog>
  );
}
