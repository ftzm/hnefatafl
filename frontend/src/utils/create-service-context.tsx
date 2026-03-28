import { createContext, type ParentComponent, useContext } from "solid-js";

export function createServiceContext<T>(name: string) {
  const ctx = createContext<T>();

  function use(): T {
    const value = useContext(ctx);
    if (!value)
      throw new Error(`use${name} must be used within a ${name} provider`);
    return value;
  }

  function createProvider(factory: () => T): ParentComponent {
    return (props) => {
      const service = factory();
      return <ctx.Provider value={service}>{props.children}</ctx.Provider>;
    };
  }

  return { use, createProvider } as const;
}
