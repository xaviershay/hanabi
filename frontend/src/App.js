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
    }
    this.handleClick = this.handleClick.bind(this)
  }

  handleClick() {
    let newCards = this.state.cards.map(card => {
      if (card.id === 1) {
        return Object.assign({}, card, {location: ['hand', 'Xavier'], rank: 3, color: 'blue'})
      } else {
        return card
      }
    })
    this.setState({cards: newCards})
  }

  render() {
    return <div>
      <h1>Hanabi</h1>
      <div class='new-board'>
        <div class='card--container flipped' style={{left: 20, top: 240}}>
          <div class='face card--back'>
          </div>
          <div class='face card--front'>
            <div style={{ opacity: 0.5}}><i class='fas fa-leaf'></i></div>
          </div>
        </div>

        <div class='card--container' style={{left: 120, top: 240}}>
          <div class='face card--back'>
          </div>
          <div class='face card--front green card-placeholder'>
            <div style={{ opacity: 0.5}}><i class='fas fa-leaf'></i></div>
          </div>
        </div>

        <div class='card--container' style={{left: 220, top: 240}}>
          <div class='face card--back'>
          </div>
          <div class='face card--front red card-number'>
            <div class="top-number small-number">
              <div>2</div>
              <div>2</div>
            </div>
            <div class="card--icons card--icons-2">
              <span>
                <i class='fas fa-heart'></i>
                <i class='fas fa-heart'></i>
              </span>
            </div>
            <div class="bottom-number small-number">
              <div>2</div>
              <div>2</div>
            </div>
          </div>
        </div>
      </div>
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
