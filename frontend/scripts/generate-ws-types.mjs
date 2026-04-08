import { TypeScriptGenerator } from "@asyncapi/modelina";
import { readFileSync, writeFileSync, mkdirSync } from "fs";

const spec = JSON.parse(
  readFileSync("../backend/asyncapi.json", "utf-8"),
);

const generator = new TypeScriptGenerator({
  modelType: "interface",
  enumType: "union",
});

const models = await generator.generate(spec);

mkdirSync("src/api/generated/ws", { recursive: true });

let output = "// This file was auto-generated from asyncapi.json. Do not edit.\n\n";

for (const model of models) {
  output += model.result + "\n\n";
}

writeFileSync("src/api/generated/ws/index.ts", output);
console.log(
  `Generated ${models.length} models to src/api/generated/ws/index.ts`,
);
