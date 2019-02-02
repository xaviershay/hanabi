import React, { Component } from 'react';
import './App.css';
import Board from './Board.js';

class App extends Component {
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

  constructor(props) {
    super(props)
    this.state = {
      value: [5, 10, 1, 3],
      cards: [
        {cardId: 1, rank: 2, color: 'red', location: ['hand', 'Jared']},
        {cardId: 2, rank: 2, color: 'red', location: ['hand', 'Jared']},
        {cardId: 3, rank: 3, color: 'white', location: ['hand', 'Jared']},
        {cardId: 4, rank: 4, color: 'yellow', location: ['hand', 'Jared']},
        {cardId: 5, rank: 5, color: 'green', location: ['hand', 'Jared']},

        {cardId: 6, location: ['hand', 'Xavier']},
        {cardId: 7, location: ['hand', 'Xavier']},
        {cardId: 8, location: ['hand', 'Xavier']},
        {cardId: 9, location: ['hand', 'Xavier']},
        {cardId: 10, location: ['hand', 'Xavier']},

        {cardId: 100, location: ['deck']},
        {cardId: 101, location: ['deck']},
        {cardId: 102, location: ['deck']},

        {cardId: 202, rank: 1, color: 'white', location: ['table']},
        {cardId: 203, rank: 4, color: 'green', location: ['table']},
        {cardId: 204, rank: 3, color: 'green', location: ['table']}
      ]
    }
    this.handleClick = this.handleClick.bind(this)
  }

  handleClick() {
    let newCards = this.state.cards.map(card => {
      if (card.cardId == 100) {
        return Object.assign({}, card, {location: ['hand', 'Jared'], rank: 3, color: 'blue'})
      } else {
        return card
      }
    })
    this.setState({cards: newCards})
  }

  render() {
    console.log(this.state)
    return <div>
      <h1>Hanabi</h1>
      <Board data={this.state.cards} size={[500,500]} />
      <button onClick={() => this.handleClick()}>Test</button>
    </div>
    /*
    return <div class='board' style={{position:"relative",width:"500px", height: "500px"}}>
      <div class='card red' style={{left: 0, top: 5}}>
        <div class='icons'>
          <span>
          <i class="fas fa-heart"></i>
          <i class="fas fa-heart"></i>
          <i class="fas fa-heart"></i>
          <i class="fas fa-heart"></i>
          <i class="fas fa-heart"></i>
      </span>
        </div>
        <div class='number'>5</div>
      </div>
      <div class='card green' style={{left: 80, top: 5}}>
        <div class='icons'>
          <span>
          <i class="fas fa-leaf"></i>
          <i class="fas fa-leaf"></i>
          </span>
        </div>
        <div class='number'>2</div>
      </div>
      <div class='card yellow' style={{left: 160, top: 5}}>
        <div class='icons'>
          <span>
            <i class="fas fa-bolt"></i>
            <i class="fas fa-bolt"></i>
            <i class="fas fa-bolt"></i>
          </span>
        </div>
        <div class='number'>3</div>
      </div>
      <div class='card blue' style={{left: 240, top: 5}}>
        <div class='icons'>
          <span>
          <i class="fas fa-tint"></i>
          </span>
        </div>
        <div class='number'>1</div>
      </div>
      <div class='card white' style={{left: 320, top: 5}}>
        <div class='icons'>
          <span>
            <i class="fas fa-balance-scale"></i>
            <i class="fas fa-balance-scale"></i>
            <i class="fas fa-balance-scale"></i>
            <i class="fas fa-balance-scale"></i>
          </span>
        </div>
        <div class='number'>4</div>
      </div>
      <div class='card white' style={{left: 0, top: 115}}>
        <div style={{opacity: 0.3}}>?</div>
      </div>
      <div class='card white' style={{left: 80, top: 115}}>
        <div style={{opacity: 0.5}}><i class='fas fa-balance-scale'></i></div>
      </div>
      <div class='card green' style={{left: 240, top: 115}}>
        <div style={{opacity: 0.5}}><i class='fas fa-leaf'></i></div>
      </div>
      <div class='card white' style={{left: 160, top: 115}}>
        <div class='icons' style={{opacity: 0.3}}>
          <span>
            <i class="fas fa-question"></i>
            <i class="fas fa-question"></i>
      <br/>
            <i class="fas fa-question"></i>
            <i class="fas fa-question"></i>
          </span>
        </div>
        <div style={{opacity: 0.3}} class='number'>4</div>
      </div>
      <div class='card white' style={{left: 320, top: 115}}>
        <div class='icons' style={{opacity: 0.5}}>
          <span>
            <i class="fas fa-heart"></i>
            <i class="fas fa-leaf" style={{visibility: "hidden"}}></i>
            <i class="fas fa-bolt"></i>
            <i class="fas fa-tint"></i>
            <i class="fas fa-balance-scale"></i>
          </span>
        </div>
        <div class='number' style={{marginTop: "5px", opacity: 0.3, fontSize: "14px", fontWeight: 900}}><span style={{visibility: "hidden"}}>1</span> 2 3<br />
      <span style={{visibility: "hidden"}}>4</span> 5</div>
      </div>
      <div class='card' style={{left: 0, top: 225}}>
      </div>
    </div>
    */
  }
}

export default App;
