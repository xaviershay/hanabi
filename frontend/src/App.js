import React, { Component } from 'react';
import './App.css';
import Board from './Board.js';

class App extends Component {
  componentDidMount() {
    let version = 0;
    let f = () => {
      fetch('http://localhost:8080/games/1?version=' + version + "&" + (new Date()).getTime())
        .then(results => results.json())
        .then(data => {
          console.log(data)
          this.setState({cards: data.cards})
          version = data.version;
          f();
        })
    }

    f()
  }

  constructor(props) {
    super(props)
    this.state = {
      cards: [
        {id: 1,  location: ['deck']},
        {id: 2,  location: ['deck']},
        {id: 3,  location: ['deck']},
        {id: 4,  location: ['deck']},
        {id: 5,  location: ['deck']},
        {id: 6, location: ['hand', 'Xavier']},
        {id: 7, location: ['hand', 'Xavier']},
        {id: 8, location: ['hand', 'Xavier']},
        {id: 9, location: ['hand', 'Xavier']},
        {id: 10, location: ['hand', 'Xavier']},
        {id: 100, location: ['deck']},
        {id: 101, location: ['deck']},
        {id: 102, location: ['deck']},
        {id: 202, rank: 1, color: 'white', location: ['table']},
        {id: 203, rank: 4, color: 'green', location: ['table']},
        {id: 204, rank: 3, color: 'green', location: ['table']},
        {id: 881, rank: 2, color: 'blue',   location: ['discard']},
        {id: 882, rank: 2, color: 'blue',   location: ['discard']},
        {id: 883, rank: 3, color: 'white',  location: ['discard']},
        {id: 884, rank: 4, color: 'yellow', location: ['discard']},
        {id: 885, rank: 5, color: 'green',  location: ['discard']},
        {id: 886, rank: 4, color: 'green',  location: ['discard']},
        {id: 887, rank: 4, color: 'green',  location: ['discard']}
      ]
    }
    this.handleClick = this.handleClick.bind(this)
  }

  handleClick() {
    /*
    let newCards = this.state.cards.map(card => {
      if (card.id === 1) {
        return Object.assign({}, card, {location: ['hand', 'Xavier'], rank: 3, color: 'blue'})
      } else {
        return card
      }
    })
    this.setState({cards: newCards})
    */
    let newCards = 
      [
        {id: 1, rank: 2, color: 'red', location: ['hand', 'Jared']},
        {id: 2, rank: 2, color: 'red', location: ['hand', 'Jared']},
        {id: 3, rank: 3, color: 'white', location: ['hand', 'Jared']},
        {id: 4, rank: 4, color: 'yellow', location: ['hand', 'Jared']},
        {id: 5, rank: 5, color: 'green', location: ['hand', 'Jared']},
        {id: 6, location: ['hand', 'Xavier']},
        {id: 7, location: ['hand', 'Xavier']},
        {id: 8, location: ['hand', 'Xavier']},
        {id: 9, location: ['hand', 'Xavier']},
        {id: 10, location: ['hand', 'Xavier']},
        {id: 100, location: ['deck']},
        {id: 101, location: ['deck']},
        {id: 102, location: ['deck']},
        {id: 202, rank: 1, color: 'white', location: ['table']},
        {id: 203, rank: 4, color: 'green', location: ['table']},
        {id: 204, rank: 3, color: 'green', location: ['table']},
        {id: 881, rank: 2, color: 'blue',   location: ['discard']},
        {id: 882, rank: 2, color: 'blue',   location: ['discard']},
        {id: 883, rank: 3, color: 'white',  location: ['discard']},
        {id: 884, rank: 4, color: 'yellow', location: ['discard']},
        {id: 885, rank: 5, color: 'green',  location: ['discard']},
        {id: 886, rank: 4, color: 'green',  location: ['discard']},
        {id: 887, rank: 4, color: 'green',  location: ['discard']}
      ]
    /*
    let newCards = this.state.cards.map(card => {
      if (card.id === 100) {
        return Object.assign({}, card, {location: ['table'], rank: 1, color: 'blue'})
      } else if (card.id == 1) {
        return Object.assign({}, card, {location: ['discard']})
      } else {
        return card
      }
    })
    */
    this.setState({cards: newCards})
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
