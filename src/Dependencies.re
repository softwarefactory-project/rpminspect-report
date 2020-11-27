// A module type to define the `fetch` capability
module type Fetcher = {
  let fetch: string => Js.Promise.t(option(Js.Json.t));
  let post:
    (string, option(Js.Json.t)) =>
    Js.Promise.t(Belt.Result.t(option(Js.Json.t), string));

  let put:
    (string, option(Js.Json.t)) =>
    Js.Promise.t(Belt.Result.t(option(Js.Json.t), string));
  let delete: string => Js.Promise.t(Belt.Result.t(Fetch.response, string));
};
