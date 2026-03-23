import type { ParentProps } from "solid-js";

interface FormFieldProps extends ParentProps {
  label: string;
  class?: string;
}

export default function FormField(props: FormFieldProps) {
  return (
    <div class={`form-field${props.class ? ` ${props.class}` : ""}`}>
      <span class="form-field-label">{props.label}</span>
      {props.children}
    </div>
  );
}
