open Patternfly;
open Patternfly.Layout;


module Main = (Fetcher: Dependencies.Fetcher) => {

  [@react.component]
  let make = () =>
  <p>{"Hello" |> React.string } </p>
};
