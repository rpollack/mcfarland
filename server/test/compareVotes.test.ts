import request from "supertest";
import { beforeEach, afterEach, describe, expect, it } from "vitest";
import { createServer } from "../src/index.js";
import { __resetCompareVotesForTests, __setCompareVoteStoreForTests, buildMatchupKey } from "../src/compareVotes.js";
import { listPlayers } from "../src/dataStore.js";

const app = createServer();
const [first, second, third] = listPlayers("hitter").map((player) => String(player.PlayerId));
const voterA = "voter-a-0123456789abcdef";
const voterB = "voter-b-0123456789abcdef";

function getVotes(ids: string[], voterId?: string) {
  const req = request(app).get("/api/compare/votes").query({ type: "hitter", ids: ids.join(",") });
  return voterId ? req.set("X-Voter-Id", voterId) : req;
}

function postVote(ids: string[], choice: string, voterId?: string) {
  const req = request(app).post("/api/compare/votes").send({ playerType: "hitter", playerIds: ids, choice });
  return voterId ? req.set("X-Voter-Id", voterId) : req;
}

describe("compare votes", () => {
  beforeEach(() => {
    __resetCompareVotesForTests();
  });

  afterEach(() => {
    __setCompareVoteStoreForTests(null);
  });

  it("builds the same matchup key regardless of player order", () => {
    expect(buildMatchupKey("hitter", ["b", "a"])).toBe(buildMatchupKey("hitter", ["a", "b"]));
    expect(buildMatchupKey("hitter", ["a", "b"])).not.toBe(buildMatchupKey("pitcher", ["a", "b"]));
  });

  it("hides the split until the voter has voted", async () => {
    await postVote([first, second], first, voterB);

    const response = await getVotes([first, second], voterA);

    expect(response.status).toBe(200);
    expect(response.body).toEqual({ yourVote: null, counts: null, total: 0 });
  });

  it("records a vote and reveals the split to the voter", async () => {
    const response = await postVote([first, second], first, voterA);

    expect(response.status).toBe(200);
    expect(response.body).toEqual({ yourVote: first, counts: { [first]: 1 }, total: 1 });
  });

  it("tallies votes from different voters, whatever order the players are listed in", async () => {
    await postVote([first, second], first, voterA);
    const response = await postVote([second, first], second, voterB);

    expect(response.body.counts).toEqual({ [first]: 1, [second]: 1 });
    expect(response.body.total).toBe(2);
    expect(response.body.yourVote).toBe(second);

    const lookup = await getVotes([first, second], voterA);
    expect(lookup.body).toEqual({ yourVote: first, counts: { [first]: 1, [second]: 1 }, total: 2 });
  });

  it("keeps the first vote from a voter", async () => {
    await postVote([first, second], first, voterA);
    const response = await postVote([first, second], second, voterA);

    expect(response.body).toEqual({ yourVote: first, counts: { [first]: 1 }, total: 1 });
  });

  it("supports three-player matchups", async () => {
    const response = await postVote([first, second, third], third, voterA);

    expect(response.body.counts).toEqual({ [third]: 1 });
  });

  it("rejects bad requests", async () => {
    expect((await postVote([first, second], third, voterA)).status).toBe(400);
    expect((await postVote([first, first], first, voterA)).status).toBe(400);
    expect((await postVote([first], first, voterA)).status).toBe(400);
    expect((await postVote([first, second], first)).status).toBe(400);
    expect((await postVote([first, second], first, "short")).status).toBe(400);
    expect((await getVotes([first, second])).status).toBe(400);
  });

  it("404s for unknown players", async () => {
    expect((await postVote([first, "does-not-exist"], first, voterA)).status).toBe(404);
    expect((await getVotes([first, "does-not-exist"], voterA)).status).toBe(404);
  });

  it("reports voting as unavailable when storage fails", async () => {
    __setCompareVoteStoreForTests({
      cast: async () => {
        throw new Error("db down");
      },
      counts: async () => {
        throw new Error("db down");
      },
      voteOf: async () => {
        throw new Error("db down");
      },
    });

    expect((await postVote([first, second], first, voterA)).status).toBe(503);
    expect((await getVotes([first, second], voterA)).status).toBe(503);
  });
});
