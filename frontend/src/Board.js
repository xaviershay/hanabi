import React, { Component } from 'react'
import * as d3array from 'd3-array'
import * as d3 from 'd3'

function px(n) {
  return n + "px"
}

function iconFor(color) {
  return {
    'red': 'fas fa-heart',
    'blue': 'fas fa-tint',
    'yellow': 'fas fa-bolt',
    'green': 'fas fa-leaf',
    'white': 'fas fa-balance-scale'
  }[color]
}

class Board extends Component {
  constructor(props){
    super(props)
    this.createBoard = this.createBoard.bind(this)
  }
  componentDidMount() {
    this.createBoard()
  }
  componentDidUpdate() {
    this.createBoard()
  }

  createBoard() {
    const node = this.node
    const rawData = this.props.data

    const data =
      Array.from(d3array.group(rawData, d => d.location.join('-')).values())
        .flatMap(cs => cs.sort((x, y) => d3.ascending(x.rank, y.rank)).map((c, i) => Object.assign({}, c, {index: i})))

    let allColors = ['red', 'green', 'yellow', 'blue', 'white']

    allColors.forEach(color => {
      data.push({cardId: '0-' + color, rank: 0, color: color, location: ['table']})
    })

    let handLocations = d3.set(data.filter(d => d.location[0] === 'hand'), d => d.location.join('-')).values()
    let discardCards = data
      .filter(d => d.location[0] === 'discard')
      .sort((x, y) => d3.ascending(x.rank, y.rank))
      .map((c, i) => Object.assign({}, c, {index: i}))

    let cardsInDeck = data.filter(d => d.location[0] === 'deck').length
    data.push({cardId: 'deck-marker', location: ['deck']})

    let cardWidth = 70
    let cardHeight = 100
    let margin = 20

    let spacer = 10
    let handRowHeight = cardHeight + 40
    let colSpacer = 4
    let handsMarginY = margin + cardHeight + (spacer * 5)
    let deckMarginY = handsMarginY + handLocations.length * handRowHeight + spacer
    let discardMarginY = deckMarginY + cardHeight + spacer
    let boardWidth = (cardWidth + colSpacer) * 5 + colSpacer + 2 * margin
    let boardHeight = discardMarginY + cardHeight + margin

    let tableX = d3.scaleOrdinal()
      .domain(allColors)
      .range(allColors.map((d, i) => margin + i * (cardWidth + colSpacer) + (colSpacer / 2)))

    let tableY = d3.scaleOrdinal()
      .domain([0])
      .range([margin, margin + cardHeight])

    let handPositions = [0, 1, 2, 3, 4]
    let handsX = d3.scaleOrdinal()
      .domain(handPositions)
      .range(handPositions.map((d, i) => margin + i * (cardWidth + colSpacer) + (colSpacer / 2)))

    let handsY = d3.scaleOrdinal()
      .domain(handLocations)
      .range(handLocations.map((d, i) => handsMarginY + handRowHeight * i))

    let playerY = d3.scaleOrdinal()
      .domain(handLocations)
      .range(handLocations.map((d, i) => handsMarginY + handRowHeight * i + cardHeight))

    let deckX = d3.scaleOrdinal()
      .domain([0])
      .range([margin + 2 * (cardWidth + colSpacer) + colSpacer / 2, margin + 3 * (cardWidth + colSpacer) + colSpacer / 2])

    let deckY = d3.scaleOrdinal()
      .domain([0])
      .range([deckMarginY, deckMarginY + cardHeight])

    let extent = d3.extent(discardCards, d => d.index)
    let range = [margin + colSpacer, boardWidth - margin - cardWidth - (colSpacer / 2)]
    let discardX = d3.scaleSequential(d3.interpolateBasis([range[0], range[1]]))
      .domain(extent)

    let discardY = d3.scaleOrdinal()
      .domain([0])
      .range([discardMarginY, discardMarginY + cardHeight])

    let t = d3.select(node).transition().duration(750)

    let xFor = d => {
      switch (d.location[0]) {
        case "hand": return handsX(d.index);
        case "table": return tableX(d.color);
        case "deck": return deckX(0);
        case "discard": return discardX(d.index);
        default: return null;
      }
    }
    let yFor = d => {
      switch (d.location[0]) {
        case "hand": return handsY(d.location.join('-'));
        case "table": return tableY(0);
        case "deck": return deckY(0);
        case "discard": return discardY(0);
        default: return null;
      }
    }
// Re-populate the contents of a card. This is currently not animated at all,
// may need to un-DRY this if we want to do that.
function fillCard(card) {
  card.html("")
  card
    .filter(d => d.rank === 0)
    .append('div')
    .append('i')
      .style('opacity', 0.4)
      .attr('class', d => iconFor(d.color))

  let paddingFor = d => {
    if (d.rank === 3 || d.rank === 4) {
      if (d.color === 'yellow') {
        return px(20)
      } else {
        return px(12)
      }
    }
    return null
  }
  card
    .filter(d => d.rank > 0)
    .attr('class', d => ['card', d.color].join(' '))
    .call(card => {
      card.append('div')
        .attr('class', d=> ['icons', 'icons-' + d.color].join(' '))
        .style('padding-left', paddingFor)
        .style('padding-right', paddingFor)
        .append('span')
          .selectAll('i')
          .data(d => Array.from({length: d.rank}, (v, k) => {return {color: d.color, n: k+1}}), d => d.n)
            .join('i')
            .attr('class', d => iconFor(d.color))
      card.append('div')
        .attr('class', 'number')
        .text(d => d.rank)
    })
  // TODO: Using mouseover here is pretty lame, and doesn't work at all on
  // touch devices.
  card
    .filter(d => d.location[0] === 'discard')
    .on("mouseover", function(d, i) {
      d3.select(this)
        .style('z-index', 9000)
    })
    .on("mouseout", function(d, i){
      d3.select(this)
        .style('z-index', d => d.index)
    })
}


    d3.select(node)
      .style('width', px(boardWidth))
      .style('height', px(boardHeight))
      .selectAll('.card')
        .data(data, d => d.cardId)
        .join(
          enter => {
            let card = enter.append('div')
              .attr('class', d => ['card', d.color].join(' '))
              .style('width', d => px(cardWidth))
              .style('height', d => px(cardHeight))
              .style('left', d => px(deckX(0)))
              .style('top', d => px(deckY(0)))
              .style('z-index', d => 100 + d.rank)
              .call(enter => enter.transition(t)
                .style('left', d => px(xFor(d)))
                .style('top', d => px(yFor(d)))
              )
            card
              .filter(d => d.cardId === 'deck-marker')
              .attr('class', 'card deck-marker')
              .attr('z-index', 1000)

            card
              .filter(d => d.location[0] === 'discard')
              .style('z-index', d => d.index)

            card
              .filter(d => d.rank !== undefined && d.color)
              .call(fillCard)
          }
          ,
          update => {
            update
              .style('z-index', d => d.rank)
              .call(update => {
                update.transition(t)
                  .style('left', d => px(xFor(d)))
                  .style('top', d => px(yFor(d)))
                update
                  .filter(d => d.rank !== undefined && d.color)
                  .call(fillCard)
              })
          }
        )

    d3.select('.deck-marker').text(d => cardsInDeck)

    let extractPlayer = d => {
      if (d.location[0] === 'hand') {
        return d.location[1]
      } else {
        return null
      }
    }
    d3.select(node)
      .selectAll('.playerLabel')
      .data(d3.set(data.map(extractPlayer).filter(d => d)).values())
        .join('div')
          .attr('class', 'playerLabel')
          .style('width', px(boardWidth - margin*2))
          .style('left', d => px(margin))
          .style('top', d => px(playerY(d)))
          .text(d => d)
  }
  render() {
    return <div className='board' id='board' ref={node => this.node = node}></div>
  }
}
export default Board
