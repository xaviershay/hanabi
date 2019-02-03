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

function makeRange(n) {
  return Array.from({length: n}, (v, k) => k+1)
}

function trace(x) {
  console.log(x)
  return x
}

function isFlipped(d) {
  return d.rank === undefined
}

function flipCard(d) {
  if (isFlipped(d)) {
    d3.select(this)
      /* Order of operations important! Translate has to come first. */
      .style('transform', 'translate3d(0, -100%, 1px) rotateX(-180deg)')
  }
}

function createNumberCardFront(front, d) {
  front
    // TODO: face and card--front classes are duplicated here and in
    // caller
    .attr('class', [
      'face',
      'card--front',
      'card--color-' + d.color,
      'card--rank-' + d.rank,
      'card-number'
    ].join(' '))

  front.append('div')
    .attr('class', 'top-number small-number')
    .call(div => {
      div.append('div').text(d.rank)
      div.append('div').text(d.rank)
    })

  front.append('div')
    .attr('class', 'card--icons')
    .append('span')
      .selectAll('i')
      .data(
        d => makeRange(d.rank).map(n => { return {color: d.color, n: n}}),
        d => d.n
      )
        .join('i')
        .attr('class', d => iconFor(d.color))

  front.append('div')
    .attr('class', 'bottom-number small-number')
    .call(div => {
      div.append('div').text(d.rank)
      div.append('div').text(d.rank)
    })
}

function createPlaceholderCardFront(front, d) {
  front
    // TODO: face and card--front classes are duplicated here and in
    // caller
    .attr('class', [
      'face',
      'card--front',
      'card--color-' + d.color,
      'card-placeholder'
    ].join(' '))

  front.append('i').attr('class', d => iconFor(d.color))
}

function createCardFront(d) {
  let front = d3.select(this)
  //console.log(front, d)

  if (d.rank !== undefined && d.color) {
    if (d.rank > 0) {
      createNumberCardFront(front, d)
    } else {
      createPlaceholderCardFront(front, d)
    }
  }
}

class Board extends Component {
  constructor(props){
    super(props)
    this.createBoard = this.createBoard.bind(this)
    this.previousData = d3.local()
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

    if (rawData.length === 0) {
      return
    }

    let data =
      Array.from(d3array.group(rawData, d => d.location.join('-')).values())
        .flatMap(cs => cs.sort((x, y) => d3.ascending(x.rank, y.rank)).map((c, i) => Object.assign({}, c, {index: i})))

    let allColors = ['red', 'green', 'yellow', 'blue', 'white']

    allColors.forEach(color => {
      data.push({id: '0-' + color, rank: 0, color: color, location: ['table']})
    })

    let highestRankFor = new Map()
    d3array
      .group(data.filter(c => c.location[0] == 'table'), d => d.color)
      .forEach((cs, color) => 
        highestRankFor.set(color, d3.max(cs, d => d.rank)))

    data.forEach(card => {
      if (card.location[0] === 'table') {
        if (card.rank == highestRankFor.get(card.color))  {
          card.stackIndex = 0
        } else {
          card.stackIndex = -1 
        }
      }
    })

    //let handLocations = d3.set(data.filter(d => d.location[0] === 'hand'), d => d.location.join('-')).values()
    let handLocations = ['hand-Jared', 'hand-Xavier']
    let discardCards = data
      .filter(d => d.location[0] === 'discard')
      .sort((x, y) => d3.ascending(x.rank, y.rank))
      .map((c, i) => Object.assign({}, c, {index: i, stackIndex: i}))

    let cardsInDeck = data.filter(d => d.location[0] === 'deck').length
    data.push({id: 'deck-marker', location: ['deck']})

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
    console.log(deckX("Jared"), deckX("Xavier"))

    let extent = d3.extent(discardCards, d => d.index)
    let range = [margin + colSpacer, boardWidth - margin - cardWidth - (colSpacer / 2)]
    let discardX = d3.scaleSequential(d3.interpolateBasis([range[0], range[1]]))
      .domain(extent)

    let discardY = d3.scaleOrdinal()
      .domain([0])
      .range([discardMarginY, discardMarginY + cardHeight])

    let t = d3.select(node).transition()
      .duration(2000)

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

    let translate3d = (x, y, z) => "translate3d(" + [x,y,z].join(',') + ")"
    let deg = n => n + "deg"

    let calculateTransform = d => {
      let x = xFor(d)
      let y = yFor(d)
      let z = d.stackIndex || 0

      let rx = 0
      //let ry = 0
      //let rz = 0

      if (isFlipped(d)) {
        y = y - cardHeight
        rx = -180
      }

      return [
        translate3d(px(x), px(y), px(z)),
        "rotateX(" + deg(rx) + ")"
      ].join(' ')
    }

    let previousData = this.previousData

    d3.select(node)
      .style('width', px(boardWidth))
      .style('height', px(boardHeight))
      .selectAll('.card--container')
        .data(data, d => d.id)
        .call(card => {
          card.join(
            enter => {
              enter.append('div')
                .attr('class', 'card--container')
                .call(container => {
                  container.append('div')
                    .attr('class', 'face card--back')
                  container.append('div')
                    .attr('class', 'face card--front')
                    .each(createCardFront)
                })
                .style('transform', calculateTransform)
            },
            update => {
              let delayCounter = 0

              // Animate left/top from previous data to new data
              update
                .each(function(d) {
                  let prev = previousData.get(this)
                  let cardNode = d3.select(this)
                  if (prev) {
                    let lastX = xFor(prev)
                    let lastY = yFor(prev)
                    let lastFlipped = isFlipped(prev)
                    let nowX = xFor(d)
                    let nowY = yFor(d)
                    let nowFlipped = isFlipped(d)

                    if (lastFlipped != nowFlipped) {
                      // recreate card face, since revealing new information!
                      cardNode.select('.card--front')
                        .html("")
                        .each(createCardFront)
                    }

                    if (lastX !== nowX || lastY !== nowY || lastFlipped !== nowFlipped) {
                      cardNode
                        .transition(t)
                          .delay((d, i) => delayCounter * 100)
                          .styleTween('transform', function() {
                            return function(t) {
                              // TODO: Figure out good way to DRY up with
                              // calculateTransform
                              // Base transformation
                              let x = lastX + (nowX - lastX) * t
                              let y = lastY + (nowY - lastY) * t
                              let zHeight = 40
                              let z = (Math.pow(0.5, 2) - Math.pow(t - 0.5, 2)) * 4 * zHeight

                              if (t >= 1.0) {
                                z += d.stackIndex || 0
                              }

                              let ret = translate3d(px(x), px(y), px(z))

                              if (lastFlipped && !nowFlipped) {
                                // Flip from back to front
                                return ret +
                                  "translateY(" + (-100 + t * 100) + "%) " +
                                  "rotateX(" + (-180 + t * 180) + "deg)"
                              } else if (!lastFlipped && nowFlipped) {
                                return ret +
                                  "translateY(" + (t * -100) + "%) " +
                                  "rotateX(" + (t * -180) + "deg)"
                              }
                              return ret
                            }
                          })
                      delayCounter += 1
                    }
                  } else {
                    cardNode
                      .style('transform', calculateTransform)
                  }
                })
            }
          )
        })

    // Re-grab selection to make sure we have all elements to store current
    // data as previous. It's possible to do this in the prior selection, but
    // it's subtle to get right and requires some duplication. Simpler to just
    // select again.
    //
    // Need to use a function here rather than lambda so that `this` is set
    // correctly.
    d3.select(node)
      .selectAll('.card--container')
      .each(function(d) { previousData.set(this, d); })



//// Re-populate the contents of a card. This is currently not animated at all,
//// may need to un-DRY this if we want to do that.
//function fillCard(card) {
//  card.html("")
//  card
//    .filter(d => d.rank === 0)
//    .call(card => {
//      card.append('div')
//        .attr('class', 'face face--back')
//        .style('background-color', 'purple')
//        .style('transform', 'rotateX(180deg)')
//        .text("hello")
//
//      card.append('div')
//        .attr('class', 'face face--front')
//        .style('justify-content', 'center')
//        .style('opacity', 0.4)
//        .append('div')
//        .append('i')
//          .attr('class', d => iconFor(d.color))
//    })
//
//  let paddingFor = d => {
//    if (d.rank === 3 || d.rank === 4) {
//      if (d.color === 'yellow') {
//        return px(20)
//      } else {
//        return px(12)
//      }
//    }
//    return null
//  }
//
//  card
//    .filter(d => d.rank > 0)
//    .attr('class', d => ['card', d.color].join(' '))
//    .call(card => {
//      card.append('div').attr('class', 'top-number small-number').call(top => {
//        top.append('div').text(d => d.rank)
//        top.append('div').text(d => d.rank)
//      })
//      card.append('div')
//        .attr('class', d=> ['icons', 'icons-' + d.color].join(' '))
//        .style('padding-left', paddingFor)
//        .style('padding-right', paddingFor)
//        .append('span')
//          .selectAll('i')
//          .data(d => Array.from({length: d.rank}, (v, k) => {return {color: d.color, n: k+1}}), d => d.n)
//            .join('i')
//            .attr('class', d => iconFor(d.color))
//      card.append('div').attr('class', 'bottom-number small-number').call(top => {
//        top.append('div').text(d => d.rank)
//        top.append('div').text(d => d.rank)
//      })
//    })
//  // TODO: Using mouseover here is pretty lame, and doesn't work at all on
//  // touch devices.
//  card
//    .filter(d => d.location[0] === 'discard')
//    .on("mouseover", function(d, i) {
//      d3.select(this)
//        .style('z-index', 9000)
//    })
//    .on("mouseout", function(d, i){
//      d3.select(this)
//        .style('z-index', d => d.index)
//    })
//}
//
//    let dealCount = 0
//    d3.select(node)
//      .style('width', px(boardWidth))
//      .style('height', px(boardHeight))
//      .selectAll('.card')
//        .data(data, d => d.id)
//        .join(
//          enter => {
//            let card = enter.append('div')
//              .attr('class', d => ['card', d.color].join(' '))
//              .style('width', d => px(cardWidth))
//              .style('height', d => px(cardHeight))
//              .style('z-index', d => 100 + d.rank)
//              .each(function(d) {
//                let cardNode = d3.select(this)
//                let placeCard =
//                  card => card
//                    .style('left', d => px(xFor(d)))
//                    .style('top', d => px(yFor(d)))
//                if (d.rank === 0 || d.id === 'deck-marker') {
//                  cardNode.call(placeCard)
//                } else {
//                  cardNode
//                    .style('left', d => px(deckX(0)))
//                    .style('top', d => px(deckY(0)))
//                    .call(enter => enter.transition(t)
//                      .delay((d, i) => {
//                        dealCount += 1
//                        return dealCount * 50
//                      })
//                      .call(placeCard)
//                      .style('transform-origin', 'center top')
//                      .styleTween('transform', function() {
//                        return function(t) {
//                          let zHeight = 50
//                          let z = (Math.pow(0.5, 2) - Math.pow(t - 0.5, 2)) * 4 * zHeight
//                          return "translateZ(" + z + "px) " +
//                            "translateY(" + (100 - (t * 100)) + "%) " +
//                            "rotateX(" + (t * 180 + 180) + "deg)"
//                        }
//                      })
//                    )
//                }
//              })
//            card
//              .filter(d => d.id === 'deck-marker')
//              .attr('class', 'card deck-marker')
//              .attr('z-index', 1000)
//
//            card
//              .filter(d => d.location[0] === 'discard')
//              .style('z-index', d => d.index)
//
//            card
//              .filter(d => d.rank !== undefined && d.color)
//              .call(fillCard)
//          }
//          ,
//          update => {
//            update
//              .style('z-index', d => d.rank)
//              .call(update => {
//                update.transition(t)
//                  .style('left', d => px(xFor(d)))
//                  .style('top', d => px(yFor(d)))
//                update
//                  .filter(d => d.rank !== undefined && d.color)
//                  .call(fillCard)
//              })
//          }
//        )
//
//    d3.select('.deck-marker').text(d => cardsInDeck)

    let extractPlayer = d => {
      if (d.location[0] === 'hand') {
        return d.location[1]
      } else {
        return null
      }
    }
    d3.select(node)
      .selectAll('.playerLabel')
      .data(handLocations.map(x => x.split('-')[1]))
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
