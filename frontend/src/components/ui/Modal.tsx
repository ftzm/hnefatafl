import { Dialog } from "@kobalte/core/dialog";
import type { JSX, ParentProps } from "solid-js";

interface ModalProps extends ParentProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  eyebrow: string;
  title: JSX.Element;
  subtitle?: string;
}

export default function Modal(props: ModalProps) {
  return (
    <Dialog open={props.open} onOpenChange={props.onOpenChange}>
      <Dialog.Portal>
        <Dialog.Overlay class="modal-overlay" />
        <Dialog.Content class="modal-content">
          <span class="modal-close" onClick={() => props.onOpenChange(false)}>
            &times;
          </span>
          <div class="modal-header">
            <div class="modal-eyebrow">{props.eyebrow}</div>
            <Dialog.Title class="modal-title">{props.title}</Dialog.Title>
            {props.subtitle && <div class="modal-subtitle">{props.subtitle}</div>}
          </div>
          {props.children}
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog>
  );
}
