open Jest;
open Expect;

describe("Module JsonReport", () => {
  test("Validate Json parsing", () => {
    let rawReport = Node.Fs.readFileSync("tests/result.json", `utf8);
    let parsed = App.JsonReport.decode(rawReport->Js.Json.parseExn);
    let cat1 =
      parsed
      ->Belt.Result.flatMap(r => r->App.JsonReport.getCategories->Ok)
      ->Belt.Result.getWithDefault([])
      ->Belt.List.head;
    expect(cat1) |> toEqual(Some("rpminspect"));
  })
});
