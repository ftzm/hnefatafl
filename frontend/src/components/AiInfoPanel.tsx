import Panel from "./ui/Panel";

export default function AiInfoPanel() {
  return (
    <Panel>
      <div class="form-field-label">AI Search</div>
      <div class="ai-info-stats">
        <div class="ai-stat-row">
          <span class="ai-stat-label">Depth</span>
          <span class="ai-stat-value">--</span>
        </div>
        <div class="ai-stat-row">
          <span class="ai-stat-label">Nodes</span>
          <span class="ai-stat-value">--</span>
        </div>
        <div class="ai-stat-row">
          <span class="ai-stat-label">Eval</span>
          <span class="ai-stat-value">--</span>
        </div>
        <div class="ai-stat-row">
          <span class="ai-stat-label">Time</span>
          <span class="ai-stat-value">--</span>
        </div>
      </div>
    </Panel>
  );
}
