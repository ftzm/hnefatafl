import { createSignal, For } from "solid-js";
import Button from "./ui/Button";
import Panel from "./ui/Panel";

interface ChatMessage {
  sender: string;
  text: string;
}

export default function Chat() {
  const [messages, setMessages] = createSignal<ChatMessage[]>([]);
  const [input, setInput] = createSignal("");

  let messagesRef: HTMLDivElement | undefined;

  const sendMessage = (e: SubmitEvent) => {
    e.preventDefault();
    const text = input().trim();
    if (!text) return;

    setMessages((prev) => [...prev, { sender: "You", text }]);
    setInput("");

    queueMicrotask(() => {
      if (messagesRef) messagesRef.scrollTop = messagesRef.scrollHeight;
    });
  };

  return (
    <Panel>
      <div class="chat-messages" ref={messagesRef}>
        <For each={messages()}>
          {(msg) => (
            <div class="chat-message">
              <span class="chat-sender">{msg.sender}</span> {msg.text}
            </div>
          )}
        </For>
      </div>
      <form class="chat-input" onSubmit={sendMessage}>
        <input
          type="text"
          placeholder="Type a message…"
          value={input()}
          onInput={(e) => setInput(e.currentTarget.value)}
        />
        <Button variant="solid" type="submit">
          Send
        </Button>
      </form>
    </Panel>
  );
}
