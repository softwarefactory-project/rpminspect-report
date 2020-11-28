// open Patternfly;
// open Patternfly.Layout;

open Belt;

let optionToResult =
    (v: option('a), error: Decco.decodeError)
    : Result.t('a, Decco.decodeError) =>
  switch (v) {
  | Some(v) => v->Ok
  | None => error->Error
  };
let makeDeccoError = (message, value) => Decco.{path: "", message, value};

module JsonReport = {
  [@decco]
  type result_t = {
    result: string,
    [@decco.key "waiver authorization"]
    waiverAuthorization: option(string),
    message: option(string),
    details: option(string),
    remedy: option(string),
  };

  [@decco]
  type results_t = list(result_t);

  type report_entry_t = {
    name: string,
    results: results_t,
  };

  type t = list(Belt.Result.t(report_entry_t, Decco.decodeError));

  let decode = (json: Js.Json.t): Belt.Result.t(t, Decco.decodeError) => {
    let get_results = (name: string, r_results: Js.Json.t) => {
      r_results
      ->results_t_decode
      ->Result.flatMap(results => {name, results}->Ok);
    };

    json
    ->Js.Json.decodeObject
    ->optionToResult(makeDeccoError("Unable to decode report", json))
    ->Result.flatMap(top =>
        top
        ->Js.Dict.entries
        ->Belt.List.fromArray
        ->Belt.List.map(((k, v)) => get_results(k, v))
        ->Ok
      );
  };

  let getCategories = (report: t): list(string) => {
    report
    ->Belt.List.map(re =>
        switch (re) {
        | Ok(result) => Some(result.name)
        | Error(_) => None
        }
      )
    ->Belt.List.keep(Belt.Option.isSome)
    ->Belt.List.map(Belt.Option.getExn);
  };
};

module Hook = (Fetcher: Http.Fetcher) => {
  module API = RemoteApi.API(Http.BsFetch);

  let use = (url: string) => {
    API.Hook.useAutoGet(url, JsonReport.decode);
  };
};

module Main = (Fetcher: Http.Fetcher) => {
  [@react.component]
  let make = () => <p> {"Hello" |> React.string} </p>;
};
