// A module to manage remote apis on top of RemoteData

open Belt;

// The state of a remote web data
type state_t('a) = RemoteData.t('a, option('a), string)
// The action to update the state
and action_t('a) =
  | NetworkRequestBegin
  | NetworkRequestSuccess('a)
  | NetworkRequestError(string);

// Helper types
type json_t = Js.Json.t
and result_t('a, 'b) = Result.t('a, 'b)
and decoder_t('a) = json_t => decode_t('a)
and decode_t('a) = result_t('a, Decco.decodeError)
and response_t('a) = result_t('a, string)
and promise_t('a) = Js.Promise.t('a)
and dispatch_t('a) = action_t('a) => unit;

// Helper functions
let some = x => x->Some;
let actionMap = (action: action_t('a), f: 'a => 'b): action_t('b) =>
  switch (action) {
  | NetworkRequestSuccess(v) => NetworkRequestSuccess(f(v))
  | NetworkRequestError(e) => NetworkRequestError(e)
  | NetworkRequestBegin => NetworkRequestBegin
  };

let chainCall =
    (
      start: promise_t(response_t('a)),
      next: unit => promise_t(response_t('b)),
    ) =>
  Js.Promise.(
    start
    |> then_(result => result->Belt.Result.flatMap(_ => next()->Ok)->resolve)
  );

let toLoading = (data: state_t('a)): state_t('a) =>
  // Manage transition to the Loading state:
  //   if the data was loaded, make it Loading(some(data))
  //   otherwise, make it Loading(None)
  RemoteData.(Loading(data |> map(d => Some(d)) |> withDefault(None)));

let updateRemoteData =
    (data: state_t('a), action: action_t('a)): state_t('a) =>
  // Manage transition of the state through action
  switch (action) {
  | NetworkRequestBegin => data |> toLoading
  | NetworkRequestError(error) => RemoteData.Failure(error)
  | NetworkRequestSuccess(response) => RemoteData.Success(response)
  };

let const = (a, _) => a;

let note = (o: option('a), e: 'e): result_t('a, 'e) =>
  // Convert an option to a result with a error message "note"
  switch (o) {
  | Some(v) => v->Ok
  | None => e->Error
  };

let deccoErrorToResponse = (r: decode_t('a)): response_t('a) =>
  // Convert a DeccoError to a string
  switch (r) {
  | Ok(v) => v->Ok
  // Todo: better format error
  | Error(e) => e.message->Error
  };

let responseToAction = (response: response_t('a)): action_t('a) =>
  // Convert a bs-fetch response to an action
  switch (response) {
  | Ok(r) => r->NetworkRequestSuccess
  | Error(e) => e->NetworkRequestError
  };

let getWhenNeeded = (state, dispatch, ()) => {
  // An helper for React.useEffect0 to fetch (when needed) the remote data
  switch (state) {
  | RemoteData.NotAsked => dispatch()
  | _ => ()
  };
  None;
};

// The low-level module
module API = (Fetcher: Http.Fetcher) => {
  let get = (url: string, decode: decoder_t('a), dispatch: dispatch_t('a)) => {
    dispatch(NetworkRequestBegin);
    Js.Promise.(
      Fetcher.fetch(url)
      |> then_(json =>
           json
           // TODO: improve the Fetcher to return a result when real network error happens
           ->note("Network error!")
           ->Result.flatMap(json => json->decode->deccoErrorToResponse)
           ->responseToAction
           ->dispatch
           ->resolve
         )
    );
  };

  let delete = (url: string, dispatch: dispatch_t(unit)) => {
    dispatch(NetworkRequestBegin);
    Js.Promise.(
      Fetcher.delete(url)
      |> then_(result => {
           (
             switch (result) {
             | Ok(_resp) => NetworkRequestSuccess()
             | Error(e) => e->NetworkRequestError
             }
           )
           ->dispatch;
           result->resolve;
         })
    );
  };

  let post =
      (
        url: string,
        data: option(json_t),
        decode: decoder_t('a),
        dispatch: dispatch_t('a),
      ) => {
    dispatch(NetworkRequestBegin);
    Js.Promise.(
      Fetcher.post(url, data)
      |> then_(resp =>
           resp
           ->Result.flatMap(mjson =>
               mjson
               ->note("Need json!")
               ->Result.flatMap(json => json->decode->deccoErrorToResponse)
               ->responseToAction
               ->dispatch
               ->Ok
             )
           ->resolve
         )
    );
  };

  // justPost is like post, but without answer decoding, e.g. for /auth/login
  let justPost = (url: string, data: json_t, dispatch: dispatch_t(unit)) => {
    // TODO: implement justPost with post
    dispatch(NetworkRequestBegin);
    Js.Promise.(
      Fetcher.post(url, data->Some)
      |> then_(result => {
           (
             switch (result) {
             | Ok(_resp) => NetworkRequestSuccess()
             | Error(e) => e->NetworkRequestError
             }
           )
           ->dispatch;
           result->resolve;
         })
    );
  };

  // Some ready-to-use hook
  module Hook = {
    // A standalone get hook
    let useGet =
        (decoder: decoder_t('a)): (state_t('a), string => unit) => {
      let (state, setState) = React.useState(() => RemoteData.NotAsked);
      let set_state = s => setState(_prevState => s);
      let dispatch = (url: string) =>
        get(url, decoder, r => state->updateRemoteData(r)->set_state)->ignore;
      (state, dispatch);
    };

    // A standalone get hook that automatically fetch the remote data
    let useAutoGet = (url: string, decoder: decoder_t('a)): state_t('a) => {
      let (state, dispatch) = useGet(decoder);

      // Trigger get when not asked
      React.useEffect0(getWhenNeeded(state, () => dispatch(url)));
      state;
    };

    // A standalone hook similar to useAutoGet but returns a callback to perform post
    let useSimplePost =
        (url: string, decoder: decoder_t('a))
        : (state_t('a), option(json_t) => unit) => {
      let (state, setState) = React.useState(() => RemoteData.NotAsked);
      let set_state = s => setState(_prevState => s);

      React.useEffect0(
        getWhenNeeded(state, () =>
          get(url, decoder, r => state->updateRemoteData(r)->set_state)
          ->ignore
        ),
      );

      (
        state,
        obj =>
          post(url, obj, decoder, r => state->updateRemoteData(r)->set_state)
          ->ignore,
      );
    };
  };
};
