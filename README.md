# elm-gears-challenge

GitHub Actions builds this Elm 0.18 demo and deploys it with Alchemy.
Pushes to `main` deploy the existing `elm-gears-challenge` Worker at
https://elm-gears-challenge.pfp.workers.dev.

Pull requests from this repository deploy `elm-gears-challenge-pr-<number>`.
Closing a pull request removes its preview. The production Worker is retained.

The `Alchemy` workflow uses `CONVEX_ACCESS_TOKEN` for deployment state in team
`nicu`, plus `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID` for the Worker.
The app itself has no backend or stored data.

For an existing production Worker, run the workflow on `main` with `adopt=true`
and `plan_only=true` first. Review the plan, then run with `adopt=true` and
`plan_only=false`. Later pushes deploy without adoption. The production plan
rejects resource creation, deletion, replacement, or orphaning.
