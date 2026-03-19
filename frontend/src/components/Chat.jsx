import { createSignal, For } from "solid-js";
import Button from "./ui/Button.jsx";

export default function Chat() {
  const [messages, setMessages] = createSignal([]);
  const [input, setInput] = createSignal("");

  let messagesRef;

  const sendMessage = (e) => {
    e.preventDefault();
    const text = input().trim();
    if (!text) return;

    setMessages((prev) => [...prev, { sender: "You", text }]);
    setInput("");

    // Scroll after SolidJS flushes the new message to the DOM
    queueMicrotask(() => {
      if (messagesRef) messagesRef.scrollTop = messagesRef.scrollHeight;
    });
  };

  return (
    <div class="chat-panel">
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
          onInput={(e) => setInput(e.target.value)}
        />
        <Button variant="solid" type="submit">
          Send
        </Button>
      </form>
    </div>
  );
}
