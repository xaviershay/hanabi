import React, { Component } from 'react';
import './App.css';

class App extends Component {
  render() {
    return <p>Hello</p>
  }

  componentDidMount() {
    fetch('http://localhost:8080/_status')
      .then(results => results.json())
      .then(data => {
        console.log(data)
      })
  }
}

export default App;
