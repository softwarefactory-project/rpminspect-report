open Patternfly
open Patternfly.Layout
open Webapi.Url
open Belt

module RemoteData = RemoteAPI.RemoteData

let optionToResult = (v: option<'a>, error: Decco.decodeError): Result.t<'a, Decco.decodeError> =>
  switch v {
  | Some(v) => v->Ok
  | None => error->Error
  }
let makeDeccoError = (message, value) => {
  open Decco
  {path: "", message: message, value: value}
}

let listToReactArray = xs => xs->Belt.List.toArray->React.array

module JsonReport = {
  @decco
  type result_t = {
    result: string,
    @decco.key("waiver authorization")
    waiverAuthorization: option<string>,
    message: option<string>,
    details: option<string>,
    remedy: option<string>,
  }

  @decco
  type results_t = list<result_t>

  type report_entry_t = {
    name: string,
    results: results_t,
  }

  type t = list<Belt.Result.t<report_entry_t, Decco.decodeError>>

  let decode = (json: Js.Json.t): Belt.Result.t<t, Decco.decodeError> => {
    let get_results = (name: string, r_results: Js.Json.t) => {
      r_results->results_t_decode->Result.flatMap(results => {name: name, results: results}->Ok)
    }

    json
    ->Js.Json.decodeObject
    ->optionToResult(makeDeccoError("Unable to decode report", json))
    ->Result.flatMap(top =>
      top->Js.Dict.entries->Belt.List.fromArray->Belt.List.map(((k, v)) => get_results(k, v))->Ok
    )
  }

  let sanitize = (report: t) => {
    let hasMessage = (result: result_t) => {
      result.message->Belt.Option.isSome
    }

    let discardResultsWithNonMessages = (results: list<result_t>) => {
      results->Belt.List.keep(result => result->hasMessage)
    }
    report
    ->Belt.List.map(re =>
      switch re {
      | Ok(entry) =>
        {
          name: entry.name,
          results: entry.results->discardResultsWithNonMessages,
        }->Some
      | Error(_) => None
      }
    )
    ->Belt.List.keep(Belt.Option.isSome)
    ->Belt.List.map(Belt.Option.getExn)
    ->Belt.List.keep(entry => entry.results->Belt.List.length > 0)
  }

  type stats_t = {
    ok: int,
    info: int,
    verify: int,
    bad: int,
  }

  let filterResultsByLevel = (results: list<result_t>, level: string) =>
    results->Belt.List.keep(result => result.result == level)

  let discardResultsByLevel = (results: list<result_t>, level: string) =>
    results->Belt.List.keep(result => result.result != level)

  let getLevelStats = (report: t): stats_t => {
    let getStats = (level: string) =>
      report
      ->sanitize
      ->Belt.List.reduce(0, (acc, entry) => {
        acc + entry.results->filterResultsByLevel(level)->Belt.List.length
      })
    {
      ok: getStats("OK"),
      info: getStats("INFO"),
      verify: getStats("VERIFY"),
      bad: getStats("BAD"),
    }
  }

  let getCategories = (report: t): list<string> => {
    report->sanitize->Belt.List.map(entry => entry.name)
  }
}

module Result = {
  let details = (result: JsonReport.result_t) => {
    switch result.details {
    | Some(details) => <p> <b> {"Details: "->React.string} </b> {details->React.string} </p>
    | None => React.null
    }
  }
  let remedy = (result: JsonReport.result_t) => {
    switch result.remedy {
    | Some(remedy) => <p> <b> {"Remedy: "->React.string} </b> {remedy->React.string} </p>
    | None => React.null
    }
  }

  let message = (result: JsonReport.result_t) => {
    result.message->Belt.Option.getExn
  }

  let alertBody = (result: JsonReport.result_t) => {
    <ul> <li> {result->details} </li> <li> {result->remedy} </li> </ul>
  }

  let getTitle = (message: option<string>) => {
    <p> {message->Belt.Option.getWithDefault("No message")->React.string} </p>
  }

  @react.component
  let make = (~result: JsonReport.result_t) => {
    switch result.result {
    | "OK" => <Alert variant=#Success title={getTitle(result.message)}> {result->alertBody} </Alert>
    | "INFO" => <Alert variant=#Info title={getTitle(result.message)}> {result->alertBody} </Alert>
    | "VERIFY" =>
      <Alert variant=#Warning title={getTitle(result.message)}> {result->alertBody} </Alert>
    | "BAD" => <Alert variant=#Danger title={getTitle(result.message)}> {result->alertBody} </Alert>
    | _ => React.null
    }
  }
}

module ReportResults = {
  @react.component
  let make = (~name: string, ~results: list<JsonReport.result_t>) => {
    <Card isCompact=true>
      <CardTitle> {name->React.string} </CardTitle>
      {results
      ->Belt.List.mapWithIndex((i, result) => {
        <Result key={name ++ i->string_of_int} result />
      })
      ->listToReactArray}
      <CardBody />
    </Card>
  }
}

module Report = {
  @react.component
  let make = (~data) => {
    let (ok_cb_state, setOkCB) = React.useState(() => false)
    let (info_cb_state, setInfoCB) = React.useState(() => true)
    let (verify_cb_state, setVerifyCB) = React.useState(() => true)
    let (bad_cb_state, setBadCB) = React.useState(() => true)
    let levelsStats = data->JsonReport.getLevelStats
    let data = data->JsonReport.sanitize
    let displayRPMInspectInfo = (entryname: string, displayname: string) => {
      let command = data
      ->Belt.List.keep(entry => entry.name == "rpminspect")
      ->Belt.List.head
      ->Belt.Option.flatMap(entry => {
        entry.results
        // rpminspect entries alway get a message
        ->Belt.List.keep(result => result.message->Belt.Option.getExn == entryname)
        ->Belt.List.head
        ->Belt.Option.flatMap(result => result.details)
      })
      switch command {
      | Some(cmd) =>
        <Card isCompact=true>
          <CardTitle> {displayname->React.string} </CardTitle>
          <CardBody> {cmd->React.string} </CardBody>
        </Card>

      | None => React.null
      }
    }

    let filterByFilters = (results: JsonReport.results_t): JsonReport.results_t => {
      let filtered = []
      filtered
      ->Belt.List.fromArray
      ->Belt.List.concat(ok_cb_state ? results->JsonReport.filterResultsByLevel("OK") : list{})
      ->Belt.List.concat(info_cb_state ? results->JsonReport.filterResultsByLevel("INFO") : list{})
      ->Belt.List.concat(
        verify_cb_state ? results->JsonReport.filterResultsByLevel("VERIFY") : list{},
      )
      ->Belt.List.concat(bad_cb_state ? results->JsonReport.filterResultsByLevel("BAD") : list{})
    }

    let displayFilter = {
      let getLabel = (level: string): string => {
        let count = (level: string) =>
          switch level {
          | "OK" => levelsStats.ok
          | "INFO" => levelsStats.info
          | "VERIFY" => levelsStats.verify
          | "BAD" => levelsStats.bad
          | _ => 0
          }

        level ++ "(" ++ count(level)->string_of_int ++ ")"
      }
      <Card isCompact=true>
        <CardTitle> {"Filters"->React.string} </CardTitle>
        <CardBody>
          <Bullseye>
            <Form isHorizontal=true>
              <FormGroup isInline=true fieldId="filters">
                <Checkbox
                  onChange={(_, _) => setBadCB(_ => !bad_cb_state)}
                  isChecked=bad_cb_state
                  id="BAD"
                  label={getLabel("BAD")}
                />
                <Checkbox
                  onChange={(_, _) => setVerifyCB(_ => !verify_cb_state)}
                  isChecked=verify_cb_state
                  id="VERIFY"
                  label={getLabel("VERIFY")}
                />
                <Checkbox
                  onChange={(_, _) => setInfoCB(_ => !info_cb_state)}
                  isChecked=info_cb_state
                  id="INFO"
                  label={getLabel("INFO")}
                />
                <Checkbox
                  onChange={(_, _) => setOkCB(_ => !ok_cb_state)}
                  isChecked=ok_cb_state
                  id="OK"
                  label={getLabel("OK")}
                />
              </FormGroup>
            </Form>
          </Bullseye>
        </CardBody>
      </Card>
    }

    <Stack hasGutter=true>
      {displayRPMInspectInfo("command line", "RPMInspect command")}
      {displayRPMInspectInfo("version", "RPMInspect version")}
      displayFilter
      {data
      ->Belt.List.keep(entry => entry.name != "rpminspect")
      ->Belt.List.map(entry => {
        let results = {
          entry.results->filterByFilters
        }
        results->Belt.List.length > 0
          ? <ReportResults key={entry.name} results name={entry.name} />
          : React.null
      })
      ->listToReactArray}
    </Stack>
  }
}

module UserInput = {
  @react.component
  let make = (~url: string, ~state) => {
    let (url, setURL) = React.useState(() => url)
    <Card isCompact=true>
      <CardTitle> {"RPMInspect report URL"->React.string} </CardTitle>
      <CardBody>
        <Grid hasGutter=true>
          <GridItem span=Column._11>
            <TextInput
              id="RPMInspect report URL" value=url _type=#Text onChange={(v, _) => setURL(_ => v)}
            />
          </GridItem>
          <GridItem span=Column._1>
            <Grid hasGutter=true>
              <GridItem span=Column._4>
                <Button
                  onClick={_ =>
                    url |> String.length != 0
                      ? {
                          RescriptReactRouter.push("?url=" ++ url)
                        }
                      : ()}
                  variant=#Secondary>
                  {"get"->React.string}
                </Button>
              </GridItem>
            </Grid>
            <GridItem span=Column._3>
              {switch state {
              | RemoteData.Loading(_) => <Spinner size=#Lg />
              | _ => React.null
              }}
            </GridItem>
          </GridItem>
        </Grid>
      </CardBody>
    </Card>
  }
}

module Reporter = (Client: RemoteAPI.HTTPClient) => {
  module Hook = RemoteAPI.Hook(Client)

  let header =
    <PageHeader
      logo={"RPMInspect"->React.string}
      headerTools={<PageHeaderTools>
        <PageHeaderToolsItem>
          <Button variant=#Plain>
            <a href="https://github.com/rpminspect/rpminspect"> <Icons.Help /> </a>
          </Button>
        </PageHeaderToolsItem>
      </PageHeaderTools>}
    />
  @react.component
  let make = (~url: string) => {
    let state = Hook.useAutoGet(url, JsonReport.decode)
    <Page header>
      <PageSection variant=#Dark> <UserInput url state /> </PageSection>
      <PageSection variant=#Default>
        {switch state {
        | RemoteData.NotAsked | RemoteData.Loading(_) => React.null
        | RemoteData.Success(data) => <Report data />
        | RemoteData.Failure(err) =>
          <Alert title={("Unable to load the report: " ++ err)->React.string} variant=#Danger />
        }}
      </PageSection>
    </Page>
  }
}

module Main = (Client: RemoteAPI.HTTPClient) => {
  module Reporter' = Reporter(Client)

  @react.component
  let make = () => {
    let qs = RescriptReactRouter.useUrl().search
    let url = URLSearchParams.make(qs) |> URLSearchParams.get("url")
    switch url {
    | Some(url') => <Reporter' url=url' />
    | None => <Reporter' url="" />
    }
  }
}
