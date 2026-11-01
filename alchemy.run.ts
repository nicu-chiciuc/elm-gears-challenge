import * as Cloudflare from "@alchemy.run/cloudflare";
import * as Convex from "@samebase/alchemy-convex";
import * as Alchemy from "alchemy";
import * as Effect from "effect/Effect";

export default Alchemy.Stack(
  "nicu-chiciuc/elm-gears-challenge",
  {
    providers: Cloudflare.providers(),
    state: Convex.state({ team: "nicu" }),
  },
  Effect.gen(function* () {
    const { stage } = yield* Alchemy.Stack;
    const site = yield* Cloudflare.Worker("Site", {
      name: stage === "prod" ? "elm-gears-challenge" : `elm-gears-challenge-${stage}`,
      assets: { directory: "./dist" },
      compatibility: { date: "2026-08-16" },
      workersDev: { enabled: true, previewsEnabled: true },
    }).pipe(Alchemy.RemovalPolicy.retain(stage === "prod"));

    return { url: site.url };
  }),
);
