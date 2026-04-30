import { Show } from "solid-js";

export type IncomingOfferKind = "draw" | "undo";

export interface IncomingOffer {
  kind: IncomingOfferKind;
  /** Display name of the player who initiated the offer. */
  by: string;
}

export interface BannerNotice {
  message: string;
}

interface OfferBannerProps {
  /** Persistent incoming offer; renders Accept/Decline. */
  pendingOffer?: IncomingOffer;
  /** Transient notice; renders message + dismiss. Ignored while offer pending. */
  notice?: BannerNotice;
  onAccept?: () => void;
  onDecline?: () => void;
  onDismissNotice?: () => void;
}

function offerVerb(kind: IncomingOfferKind): string {
  return kind === "draw" ? "offers a draw" : "requests undo";
}

export default function OfferBanner(props: OfferBannerProps) {
  const visible = () => !!(props.pendingOffer || props.notice);

  return (
    <Show when={visible()}>
      <div
        class="offer-banner"
        data-variant={props.pendingOffer ? "offer" : "notice"}
        role={props.pendingOffer ? "alertdialog" : "status"}
      >
        <Show
          when={props.pendingOffer}
          fallback={
            <div class="offer-banner__row">
              <span class="offer-banner__message">{props.notice?.message}</span>
              <button
                type="button"
                class="offer-banner__close"
                aria-label="Dismiss"
                onClick={() => props.onDismissNotice?.()}
              >
                &times;
              </button>
            </div>
          }
        >
          {(offer) => (
            <>
              <div class="offer-banner__message">
                {offer().by} {offerVerb(offer().kind)}
              </div>
              <div class="offer-banner__actions">
                <button
                  type="button"
                  class="offer-banner__btn"
                  onClick={() => props.onDecline?.()}
                >
                  Decline
                </button>
                <span class="offer-banner__sep" aria-hidden="true">
                  ·
                </span>
                <button
                  type="button"
                  class="offer-banner__btn"
                  onClick={() => props.onAccept?.()}
                >
                  Accept
                </button>
              </div>
            </>
          )}
        </Show>
      </div>
    </Show>
  );
}
