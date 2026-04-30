import { fireEvent, render } from "@solidjs/testing-library";
import { describe, expect, it, vi } from "vitest";
import OfferBanner from "./OfferBanner";

describe("OfferBanner", () => {
  it("renders nothing when idle", () => {
    const { container } = render(() => <OfferBanner />);
    expect(container.querySelector(".offer-banner")).toBeNull();
  });

  it("renders the offerer name and verb for a draw offer", () => {
    const { getByText } = render(() => (
      <OfferBanner pendingOffer={{ kind: "draw", by: "Ragnar" }} />
    ));
    expect(getByText(/Ragnar/)).toBeTruthy();
    expect(getByText(/offers a draw/i)).toBeTruthy();
  });

  it("renders the requester name and verb for an undo request", () => {
    const { getByText } = render(() => (
      <OfferBanner pendingOffer={{ kind: "undo", by: "Sigrid" }} />
    ));
    expect(getByText(/Sigrid/)).toBeTruthy();
    expect(getByText(/requests undo/i)).toBeTruthy();
  });

  it("calls onAccept when Accept is clicked", () => {
    const onAccept = vi.fn();
    const { getByText } = render(() => (
      <OfferBanner
        pendingOffer={{ kind: "draw", by: "Ragnar" }}
        onAccept={onAccept}
      />
    ));
    fireEvent.click(getByText("Accept"));
    expect(onAccept).toHaveBeenCalledTimes(1);
  });

  it("calls onDecline when Decline is clicked", () => {
    const onDecline = vi.fn();
    const { getByText } = render(() => (
      <OfferBanner
        pendingOffer={{ kind: "draw", by: "Ragnar" }}
        onDecline={onDecline}
      />
    ));
    fireEvent.click(getByText("Decline"));
    expect(onDecline).toHaveBeenCalledTimes(1);
  });

  it("renders a transient notice with a dismiss button", () => {
    const onDismiss = vi.fn();
    const { getByText, getByLabelText } = render(() => (
      <OfferBanner
        notice={{ message: "Draw declined" }}
        onDismissNotice={onDismiss}
      />
    ));
    expect(getByText("Draw declined")).toBeTruthy();
    fireEvent.click(getByLabelText("Dismiss"));
    expect(onDismiss).toHaveBeenCalledTimes(1);
  });

  it("hides notice content when an offer is also present", () => {
    // Offer takes priority: its message is shown, not the notice.
    const { queryByText } = render(() => (
      <OfferBanner
        pendingOffer={{ kind: "draw", by: "Ragnar" }}
        notice={{ message: "Stale notice" }}
      />
    ));
    expect(queryByText("Stale notice")).toBeNull();
    expect(queryByText(/offers a draw/i)).toBeTruthy();
  });

  it("uses role=alertdialog for offers and role=status for notices", () => {
    const { container, unmount } = render(() => (
      <OfferBanner pendingOffer={{ kind: "draw", by: "Ragnar" }} />
    ));
    expect(container.querySelector('[role="alertdialog"]')).toBeTruthy();
    unmount();

    const { container: container2 } = render(() => (
      <OfferBanner notice={{ message: "Hi" }} />
    ));
    expect(container2.querySelector('[role="status"]')).toBeTruthy();
  });
});
