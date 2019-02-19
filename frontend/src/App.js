import React, { Component } from 'react';
import './App.css';
import Board from './Board.js';

class App extends Component {
  componentDidMount() {
    let version = 0;
    let f = () => {
      fetch('http://localhost:8080/games/1?as=Xavier&version=' + version + "&" + (new Date()).getTime())
        .then(results => {
          if (!results.ok) {
            throw Error(results.statusText)
          }
          return results.json()
        })
        .then(data => {
          console.log(data)
          this.setState({cards: data.cards})
          version = data.version;
          f();
        })
        .catch(function(error) {
          console.log(error)
        })
    }

    f()
  }

  constructor(props) {
    super(props)
    this.state = {
      cards: []
    }
    this.handleClick = this.handleClick.bind(this)
  }

  handleClick() {
  }

  render() {
    return <div>
      <h1>Hanabi</h1>
      <Board data={this.state.cards} size={[500,500]} />
      <button onClick={() => this.handleClick()}>Test</button>
    </div>
  }
}

export default App;
