import React, { Component } from 'react';
import './App.css';

class App extends Component {
  render() {
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
        <div>5</div>
      </div>
      <div class='card green' style={{left: 80, top: 5}}>
        <div class='icons'>
          <span>
          <i class="fas fa-leaf"></i>
          <i class="fas fa-leaf"></i>
          </span>
        </div>
        <div>2</div>
      </div>
      <div class='card yellow' style={{left: 160, top: 5}}>
        <div class='icons'>
          <span>
            <i class="fas fa-bolt"></i>
            <i class="fas fa-bolt"></i>
            <i class="fas fa-bolt"></i>
          </span>
        </div>
        <div>3</div>
      </div>
      <div class='card blue' style={{left: 240, top: 5}}>
        <div class='icons'>
          <span>
          <i class="fas fa-tint"></i>
          </span>
        </div>
        <div>1</div>
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
        <div>4</div>
      </div>
    </div>
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
