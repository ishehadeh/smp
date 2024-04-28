#!/usr/bin/env -S deno run --allow-all

import $ from "@david/dax";
import { parseArgs } from "jsr:@std/cli/parse-args";
const CONFIG = {
  input: "progress.slides.md",
  output: {
    path: "progress.slides.html",
    type: "revealjs",
  },
  vars: {
    theme: "beige",
  },
  filters: ["pandoc/diagram.lua"],
};

const varArgs = Object.entries(CONFIG.vars)
  .flatMap(([k, v]) => ["-V", `${k}=${v}`]);
const filterArgs = CONFIG.filters.flatMap((f) => ["--lua-filter", f]);
const pandoc = () =>
  $`pandoc ${varArgs}
           ${filterArgs}
           --standalone
           --embed-resources
           ${CONFIG.input}
           --to ${CONFIG.output.type}
           --output ${CONFIG.output.path}`;

const args = parseArgs(Deno.args, {
  boolean: ["help", "watch"],
  default: { "watch": false },
});
if (args.help) {
  console.log("build-slides.ts [--watch]");
  Deno.exit(0);
}

$.cd(import.meta);

if (args.watch) {
  const watcher = Deno.watchFs(CONFIG.input);
  let runningProc = undefined;
  for await (const event of watcher) {
    if (event.kind == "modify") {
      if (runningProc) {
        runningProc.kill();
        runningProc = undefined;
      }
      runningProc = pandoc().spawn();
      runningProc.then((a) => {
        console.log(a);
        runningProc = undefined;
      }).catch((a) => {
        console.log(a);
      });
    }
  }
} else {
  await pandoc();
}
