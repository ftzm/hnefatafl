import {
  type Accessor,
  createContext,
  createSignal,
  type ParentComponent,
  useContext,
} from "solid-js";
import type { ServerError } from "./api/ws-utils";

export interface Toast {
  id: number;
  error: ServerError;
}

interface ToastContextValue {
  toasts: Accessor<Toast[]>;
  pushError: (error: ServerError) => void;
  dismiss: (id: number) => void;
}

const ToastContext = createContext<ToastContextValue>();

export const ToastProvider: ParentComponent = (props) => {
  let nextId = 0;
  const [toasts, setToasts] = createSignal<Toast[]>([]);

  function pushError(error: ServerError) {
    setToasts((prev) => [...prev, { id: nextId++, error }]);
  }

  function dismiss(id: number) {
    setToasts((prev) => prev.filter((t) => t.id !== id));
  }

  return (
    <ToastContext.Provider value={{ toasts, pushError, dismiss }}>
      {props.children}
    </ToastContext.Provider>
  );
};

export function useToasts(): ToastContextValue {
  const ctx = useContext(ToastContext);
  if (!ctx) throw new Error("useToasts must be used within ToastProvider");
  return ctx;
}
