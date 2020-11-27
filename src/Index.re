// A Fetcher implementation using `bs-fetch`
module BsFetch = {
  let handleAPICallError =
      (promise: Js.Promise.t(Fetch.Response.t))
      : Js.Promise.t(Belt.Result.t(Fetch.response, string)) => {
    promise
    |> Js.Promise.then_(r =>
         r |> Fetch.Response.ok
           ? Ok(r)->Js.Promise.resolve
           : Error("API call failed: " ++ Fetch.Response.statusText(r))
             ->Js.Promise.resolve
       );
  };

  let extractJson =
      (promise: Js.Promise.t(Belt.Result.t(Fetch.Response.t, string)))
      : Js.Promise.t(Belt.Result.t(option(Js.Json.t), string)) => {
    promise
    |> Js.Promise.then_(result =>
         switch (result) {
         | Ok(resp) =>
           resp
           |> Fetch.Response.json
           |> Js.Promise.then_(decoded =>
                Ok(decoded->Some)->Js.Promise.resolve
              )
           |> Js.Promise.catch(_ => Ok(None)->Js.Promise.resolve)
         | Error(e) => Error(e)->Js.Promise.resolve
         }
       );
  };

  let raiseOnNok = (promise: Js.Promise.t(Fetch.Response.t)) => {
    promise
    |> Js.Promise.then_(r =>
         r |> Fetch.Response.ok
           ? promise : Js.Exn.raiseError(Fetch.Response.statusText(r))
       );
  };
  let promiseToOptionalJson =
      (promise: Js.Promise.t(Fetch.response))
      : Js.Promise.t(option(Js.Json.t)) => {
    promise
    |> raiseOnNok
    |> Js.Promise.then_(Fetch.Response.json)
    |> Js.Promise.then_(v => v->Some->Js.Promise.resolve)
    |> Js.Promise.catch(e => {
         Js.log2("Unexpected error: ", e);
         None->Js.Promise.resolve;
       });
  };
  let fetch = (url: string): Js.Promise.t(option(Js.Json.t)) => {
    Fetch.fetch(url) |> promiseToOptionalJson;
  };

  let postOrPut =
      (verb, url: string, body: option(Js.Json.t))
      : Js.Promise.t(Belt.Result.t(option(Js.Json.t), string)) => {
    let headers =
      Fetch.HeadersInit.make({
        "Accept": "*",
        "Content-Type": "application/json",
        //        "Upgrade-Insecure-Requests": "1",
      });
    let req =
      switch (body) {
      | None => Fetch.RequestInit.make(~method_=verb, ~headers, ())
      | Some(json) =>
        Fetch.RequestInit.make(
          ~method_=verb,
          ~body=json->Js.Json.stringify->Fetch.BodyInit.make,
          //          ~redirect=Follow,
          //          ~mode=NoCORS,
          ~headers,
          (),
        )
      };
    Fetch.fetchWithInit(url, req) |> handleAPICallError |> extractJson;
  };
  let put = postOrPut(Put);
  let post = postOrPut(Post);
  let delete =
      (url: string): Js.Promise.t(Belt.Result.t(Fetch.response, string)) => {
    let req =
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=Fetch.HeadersInit.make({"Accept": "*"}),
        (),
      );
    Fetch.fetchWithInit(url, req) |> handleAPICallError;
  };
};

module RealApp = App.Main(BsFetch);

switch (ReactDOM.querySelector("#root")) {
| Some(root) => ReactDOM.render(<RealApp />, root)
| None => Js.log("Can't find #root element!")
};
