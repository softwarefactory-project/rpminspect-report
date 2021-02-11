// A Fetcher implementation using `bs-fetch`

module RealApp = App.Main(RemoteAPI.BsFetch);

switch (ReactDOM.querySelector("#root")) {
| Some(root) => ReactDOM.render(<RealApp />, root)
| None => Js.log("Can't find #root element!")
};
