open Belt;

type state_t('a) = RemoteData.t('a, option('a), string);

type json_t = Js.Json.t;
type result_t('a, 'b) = Result.t('a, 'b);
type decode_t('a) = result_t('a, Decco.decodeError);
type decoder_t('a) = json_t => decode_t('a);
type response_t('a) = result_t('a, string);
type promise_t('a) = Js.Promise.t('a);

let note = (o: option('a), e: 'e): result_t('a, 'e) =>
  switch (o) {
  | Some(v) => v->Ok
  | None => e->Error
  };

let extractDeccoErrorToString = (r: decode_t('a)): response_t('a) =>
  switch (r) {
  | Ok(v) => v->Ok
  | Error(e) => e.message->Error
  };

module API = (Fetcher: Http.Fetcher) => {
  let get =
      (url: string, decode: decoder_t('a), setState: state_t('a) => unit) => {
    Loading(None)->setState;
    Js.Promise.(
      Fetcher.fetch(url)
      |> then_(json =>
           json
           ->note("Network error!")
           ->Result.flatMap(json => json->decode->extractDeccoErrorToString)
           ->{
               result =>
                 switch (result) {
                 | Ok(data) => Success(data)->setState
                 | Error(err) => Failure(err)->setState
                 };
             }
           ->Ok
           ->resolve
         )
    );
  };

  module Hook = {
    let useGet =
        (url: string, decoder: decoder_t('a)): (state_t('a), unit => unit) => {
      let (state, setState) = React.useState(() => RemoteData.NotAsked);
      let set_state = s => setState(_prevState => s);
      let dispatch = () => get(url, decoder, r => r->set_state)->ignore;
      React.useEffect1(
        () => {
          dispatch();
          None;
        },
        [|url|],
      );
      (state, dispatch);
    };
  };
};
