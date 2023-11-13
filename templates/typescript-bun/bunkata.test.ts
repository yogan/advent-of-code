import { expect, test } from "bun:test";
import { square } from "./bunkata";

test("square returns 4 for 2", () => {
    expect(square(2)).toBe(4)
});
