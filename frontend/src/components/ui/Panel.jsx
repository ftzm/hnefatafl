export default function Panel(props) {
  return (
    <div class={`panel${props.class ? ` ${props.class}` : ""}`}>
      {props.children}
    </div>
  );
}
