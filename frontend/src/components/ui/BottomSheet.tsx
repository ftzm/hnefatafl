import type { ParentProps, Setter } from "solid-js";
import { Dialog } from "@kobalte/core/dialog";
import CloseIcon from "./icons/CloseIcon";

interface BottomSheetProps extends ParentProps {
  open: boolean;
  onOpenChange: Setter<boolean>;
  title: string;
}

export default function BottomSheet(props: BottomSheetProps) {
  return (
    <Dialog open={props.open} onOpenChange={props.onOpenChange}>
      <Dialog.Portal>
        <Dialog.Overlay class="bottom-sheet-overlay" />
        <Dialog.Content class="bottom-sheet-content">
          <div class="bottom-sheet-header">
            <Dialog.Title class="bottom-sheet-title">{props.title}</Dialog.Title>
            <Dialog.CloseButton class="btn btn-ghost" aria-label="Close">
              <CloseIcon />
            </Dialog.CloseButton>
          </div>
          <div class="bottom-sheet-body">
            {props.children}
          </div>
        </Dialog.Content>
      </Dialog.Portal>
    </Dialog>
  );
}
