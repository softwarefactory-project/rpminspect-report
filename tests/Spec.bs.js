'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");

Jest.describe("Dummy test", (function (param) {
        return Jest.test("dummy", (function (param) {
                      return Jest.Expect.toBe(true, Jest.Expect.expect(true));
                    }));
      }));

/*  Not a pure module */
