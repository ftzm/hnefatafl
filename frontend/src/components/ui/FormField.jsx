export default function FormField(props) {
  return (
    <div class={`form-field${props.class ? ` ${props.class}` : ""}`}>
      <label class="form-field-label">{props.label}</label>
      {props.children}
    </div>
  );
}
