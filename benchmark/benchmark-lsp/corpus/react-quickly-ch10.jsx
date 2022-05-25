import { createHashHistory, useBasename} from 'history'
import React from 'react'
import ReactDOM from 'react-dom'
import { Router, Route, IndexRoute, Link, IndexLink } from 'react-router'

const history = createHashHistory({
})

const PRODUCTS = [
  { id: 0, src: 'images/proexpress-cover.jpg', title: 'Pro Express.js', url: 'http://amzn.to/1D6qiqk' },
  { id: 1, src: 'images/practicalnode-cover.jpeg', title: 'Practical Node.js', url: 'http://amzn.to/NuQ0fM' },
  { id: 2, src: 'images/expressapiref-cover.jpg', title: 'Express API Reference', url: 'http://amzn.to/1xcHanf' },
  { id: 3, src: 'images/reactquickly-cover.jpg', title: 'React Quickly', url: 'https://www.manning.com/books/react-quickly'},
  { id: 4, src: 'images/fullstack-cover.png', title: 'Full Stack JavaScript', url: 'http://www.apress.com/9781484217504'}
]

let CartItems = {}

const Modal = React.createClass({
  styles: {
    position: 'fixed',
    top: '20%',
    right: '20%',
    bottom: '20%',
    left: '20%',
    width: 450,
    height: 400,
    padding: 20,
    boxShadow: '0px 0px 150px 130px rgba(0, 0, 0, 0.5)',
    overflow: 'auto',
    background: '#fff'
  },
  render() {
    return (
      <div style={this.styles}>
        <p><Link to={this.props.returnTo}>Back</Link></p>
        {this.props.children}
      </div>
    )
  }
})

const Heading = () => {
  return <h1>Nile Book Store</h1>
}

const Copy = () => {
  return <p>Please click on a book to view details in a modal. You can copy/paste the link of the modal. The link will open book on a separate page.</p>
}

const Cart = React.createClass ({
  render() {
    return <div>
      {(Object.keys(CartItems).length == 0) ? <p>Your cart is empty</p> : '' }
       <ul>
        {Object.keys(CartItems).map((item, index, list)=>{
          return <li key={item}>{PRODUCTS[item].title} - {CartItems[item]}</li>
        })}
      </ul>
      <Link to="/checkout" className="btn btn-primary">Checkout</Link>
      <Link to="/" className="btn btn-info">Home</Link>
    </div>
  }
})
const Checkout = React.createClass({
  render() {
    let count = 0
    return <div><h1>Invoice</h1><table className="table table-bordered"><tbody>
      {Object.keys(CartItems).map((item, index, list)=>{
        count += CartItems[item]
        return <tr key={item}>
          <td>{PRODUCTS[item].title}</td>
          <td>{CartItems[item]}</td>
        </tr>
      })}
    </tbody></table><p>Total: {count}</p></div>
  }
})

const App = React.createClass({

  componentWillReceiveProps(nextProps) {
    if (nextProps.location.key !== this.props.location.key &&
      nextProps.location.state &&
      nextProps.location.state.modal
    ) {
      this.previousChildren = this.props.children
    }
  },

  render() {
    let isModal = (this.props.location.state &&
      this.props.location.state.modal &&
      this.previousChildren
    )
    return (
      <div className="well">
        <Heading/>
        <div>

          {this.props.children}
          {(isModal)?
            <Modal isOpen={true} returnTo={this.props.location.state.returnTo}>
              {this.props.children}
            </Modal> : ''
          }
        </div>
      </div>
    )
  }
})

const Index = React.createClass ({
  render() {
    return (
      <div>
        <Copy/>
        <p><Link to="/cart" className="btn btn-danger">Cart</Link></p>
        <div>
          {PRODUCTS.map(picture => (
            <Link key={picture.id}
              to={`/products/${picture.id}`}
              state={{ modal: true, returnTo: this.props.location.pathname }}>
              <img style={{ margin: 10 }} src={picture.src} height="100" />
            </Link>
          ))}
        </div>
      </div>
    )
  }
})


const Product = React.createClass({
  handlerBuy () {
    this.props.route.handlerBuy(this.props.params.id)
  },
  render() {
    return (
      <div>
        <img src={PRODUCTS[this.props.params.id].src} style={{ height: '80%' }} />
        <p>{PRODUCTS[this.props.params.id].title}</p>
        <Link
          to={`/cart`}
          onClick={this.handlerBuy}
          state={{ productId: this.props.params.id}}
          className="btn btn-primary">
          Buy
        </Link>
      </div>
    )
  }
})

const handlerBuy = (id) => {
  if (CartItems[id])
    CartItems[id] += 1
  else
    CartItems[id] = 1
}

ReactDOM.render((
  <Router history={history}>
    <Route path="/" component={App}>
      <IndexRoute component={Index}/>
      <Route path="/products/:id" component={Product} handlerBuy={handlerBuy} />
      <Route path="/cart" component={Cart}/>
    </Route>
    <Route path="/checkout" component={Checkout}/>
  </Router>
), document.getElementById('content'))
if (false == true) undeclaredVariable.hasOwnProperty(); // Intentional error to force diagnostics.
