import { render } from "@solidjs/testing-library";
import { describe, expect, it } from "vitest";
import { ToastProvider, useToasts } from "./toast-context";

function setupContext() {
  let ctx!: ReturnType<typeof useToasts>;
  render(() => (
    <ToastProvider>
      {(() => {
        ctx = useToasts();
        return null;
      })()}
    </ToastProvider>
  ));
  return ctx;
}

describe("toast-context", () => {
  it("starts with no toasts", () => {
    const ctx = setupContext();
    expect(ctx.toasts()).toEqual([]);
  });

  it("pushError adds a toast", () => {
    const ctx = setupContext();
    ctx.pushError({ code: "invalid_move", message: "Bad move" });
    expect(ctx.toasts()).toHaveLength(1);
    expect(ctx.toasts()[0].error).toEqual({
      code: "invalid_move",
      message: "Bad move",
    });
  });

  it("stacks multiple toasts", () => {
    const ctx = setupContext();
    ctx.pushError({ code: "err1", message: "First" });
    ctx.pushError({ code: "err2", message: "Second" });
    ctx.pushError({ code: "err3", message: "Third" });
    expect(ctx.toasts()).toHaveLength(3);
    expect(ctx.toasts().map((t) => t.error.message)).toEqual([
      "First",
      "Second",
      "Third",
    ]);
  });

  it("assigns unique ids to each toast", () => {
    const ctx = setupContext();
    ctx.pushError({ code: "a", message: "A" });
    ctx.pushError({ code: "b", message: "B" });
    const ids = ctx.toasts().map((t) => t.id);
    expect(ids[0]).not.toBe(ids[1]);
  });

  it("dismiss removes a specific toast by id", () => {
    const ctx = setupContext();
    ctx.pushError({ code: "a", message: "A" });
    ctx.pushError({ code: "b", message: "B" });
    ctx.pushError({ code: "c", message: "C" });
    const idToRemove = ctx.toasts()[1].id;
    ctx.dismiss(idToRemove);
    expect(ctx.toasts()).toHaveLength(2);
    expect(ctx.toasts().map((t) => t.error.message)).toEqual(["A", "C"]);
  });

  it("dismiss with unknown id is a no-op", () => {
    const ctx = setupContext();
    ctx.pushError({ code: "a", message: "A" });
    ctx.dismiss(9999);
    expect(ctx.toasts()).toHaveLength(1);
  });

  it("throws when useToasts is called outside provider", () => {
    expect(() => {
      render(() => {
        useToasts();
        return null;
      });
    }).toThrow("useToasts must be used within ToastProvider");
  });
});
