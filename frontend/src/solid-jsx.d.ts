import "solid-js";

declare module "solid-js" {
  namespace JSX {
    interface CustomEvents {
      pointerdown: PointerEvent;
    }
  }
}
