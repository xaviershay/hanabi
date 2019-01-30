import React, { Component } from 'react';
import './App.css';

class App extends Component {
  render() {
    return <p>Hello</p>
  }

  componentDidMount() {
    let version = 0;
    let f = () => {
      fetch('http://localhost:8080/games/1?version=' + version)
        .then(results => results.json())
        .then(data => {
          console.log(data)
          version = data._gameVersion;
          f();
        })
    }

    f()
  }
}

export default App;
