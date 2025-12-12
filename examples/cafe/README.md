# Cafe Example

A cafe ordering system demonstrating event-driven workflow patterns with Eventium.

## Overview

The cafe example is inspired by [Edument's CQRS tutorial](http://cqrs.nu/tutorial) and showcases event sourcing for a restaurant/cafe ordering system. It demonstrates order management, chef todo lists, and event-based coordination between different parts of the workflow.

## Features

### Tab Management
- **Open Tab** - Start a new customer tab
- **Place Order** - Order food and drinks
- **Mark Drinks Served** - Track when drinks are delivered
- **Mark Food Prepared** - Chef marks food as ready
- **Mark Food Served** - Waiter marks food as delivered
- **Close Tab** - Complete and close the tab

### Two Applications

#### 1. Cafe Main (`cafe-main`)
Main ordering system for waiters:
- Open and manage tabs
- Place orders
- Mark items as served
- Close tabs
- View tab status

#### 2. Chef Todo List (`cafe-chef-todo-main`)
Separate read model for kitchen staff:
- View all outstanding food orders
- Mark food as prepared
- Real-time updates via event stream

### Menu System
Predefined menu with food and drinks:
- **Drinks** - Served immediately
- **Food** - Requires preparation by chef

## Architecture

```
Waiter Commands → Tab Aggregate → Events → Event Store
                                     ↓
                              [Event Bus]
                                     ↓
                         ┌───────────────────┐
                         ↓                   ↓
                    Tab View          Chef Todo List
                 (Current State)    (Outstanding Orders)
```

### Event Flow

1. **Order Placed** → `DrinksOrdered` + `FoodOrdered` events
2. **Drinks Served** → `DrinksServed` event
3. **Food Prepared** → `FoodPrepared` event (chef)
4. **Food Served** → `FoodServed` event (waiter)
5. **Tab Closed** → `TabClosed` event

## Installation

```bash
# Build both executables
cabal build examples-cafe

# Or from project root
cabal build all
```

## Usage

### Cafe Main (Waiter Interface)

```bash
# Show help
cabal run cafe-main -- --help

# Open a new tab
cabal run cafe-main -- open-tab
# Output: Opened tab. Id: 1, UUID: 550e8400-e29b-41d4-a716-446655440000

# List menu items
cabal run cafe-main -- list-menu

# Place an order (food and drinks by index)
cabal run cafe-main -- place-order --tab-id 1 --drinks 0,1 --food 0,2

# View tab status
cabal run cafe-main -- view-tab --tab-id 1

# Mark drinks as served
cabal run cafe-main -- mark-drinks-served --tab-id 1 --drink-indexes 0,1

# Mark food as prepared (chef does this)
cabal run cafe-main -- mark-food-prepared --tab-id 1 --food-indexes 0,1

# Mark food as served
cabal run cafe-main -- mark-food-served --tab-id 1 --food-indexes 0,1

# Close the tab
cabal run cafe-main -- close-tab --tab-id 1 --amount-paid 45.50
```

### Chef Todo List (Kitchen Interface)

```bash
# View outstanding food orders
cabal run cafe-chef-todo-main

# The chef todo list shows:
# - All tabs with pending food
# - Food items waiting to be prepared
# - Updates in real-time as orders come in
```

### Database Location

```bash
# Use default database (database.db)
cabal run cafe-main -- open-tab

# Specify custom database
cabal run cafe-main -- --database-path ~/cafe.db open-tab
```

## Code Structure

```
examples/cafe/
├── app/
│   ├── cafe-main.hs              # Waiter CLI entry point
│   └── cafe-chef-todo-main.hs    # Chef interface entry point
├── src/
│   └── Cafe/
│       ├── Models/
│       │   └── Tab.hs            # Tab aggregate (events, commands, projection)
│       ├── CLI/
│       │   ├── Options.hs        # Command-line parsing
│       │   └── Transformer.hs    # CLI monad transformer
│       ├── ChefTodoList.hs       # Chef read model
│       ├── CLI.hs                # Main CLI logic
│       └── DB.hs                 # Database setup
└── Cafe.hs                       # Module exports
```

## Key Concepts Demonstrated

### 1. Workflow Management
The cafe example shows how to model a multi-step workflow:
- Order → Prepare → Serve → Close

### 2. Multiple Read Models
Different views for different users:
- **Tab View** - Complete tab state for waiters
- **Chef Todo List** - Outstanding food orders for kitchen

### 3. State Transitions
Tab state evolves through events:
```haskell
data TabState = TabState
  { _tabStateIsOpen :: Bool
  , _tabStateOutstandingDrinks :: [Maybe Drink]
  , _tabStateOutstandingFood :: [Maybe Food]
  , _tabStatePreparedFood :: [Maybe Food]
  , _tabStateServedItems :: [MenuItem]
  }
```

### 4. Event-Driven Updates
Chef todo list updates automatically when:
- New orders are placed
- Food is marked as prepared
- Tabs are closed

### 5. Business Rules
- Can't serve drinks that weren't ordered
- Can't serve food before it's prepared
- Can't close tab with outstanding items
- Must pay correct amount to close tab

### 6. Lens-Based State Updates
Uses `lens` library for elegant state modifications:
```haskell
tabStateOutstandingFood .~ newFoodList
```

### 7. Dual Storage Options
Supports both:
- **In-Memory** (`eventium-memory`) - Fast, for development
- **SQLite** (`eventium-sqlite`) - Persistent, for production

## Example Workflow

```bash
# 1. Start a new tab
cabal run cafe-main -- open-tab
# Output: Opened tab. Id: 1, UUID: ...

# 2. Check the menu
cabal run cafe-main -- list-menu
# Food:
# 0: Burger ($12.00)
# 1: Pizza ($15.00)
# Drinks:
# 0: Coffee ($3.00)
# 1: Tea ($2.50)

# 3. Place an order
cabal run cafe-main -- place-order --tab-id 1 --drinks 0 --food 0

# 4. In another terminal, check chef todo list
cabal run cafe-chef-todo-main
# Shows: Tab 1 needs Burger

# 5. Serve the coffee (drinks served immediately)
cabal run cafe-main -- mark-drinks-served --tab-id 1 --drink-indexes 0

# 6. Chef prepares the burger
cabal run cafe-main -- mark-food-prepared --tab-id 1 --food-indexes 0

# 7. Serve the burger
cabal run cafe-main -- mark-food-served --tab-id 1 --food-indexes 0

# 8. Close the tab
cabal run cafe-main -- close-tab --tab-id 1 --amount-paid 15.00

# 9. View final state
cabal run cafe-main -- view-tab --tab-id 1
```

## Menu Items

### Food (Requires Preparation)
- Burger - $12.00
- Pizza - $15.00
- Salad - $8.00
- Pasta - $14.00

### Drinks (Served Immediately)
- Coffee - $3.00
- Tea - $2.50
- Soda - $2.00
- Juice - $4.00

## Technologies Used

- **Eventium Core** - Event sourcing framework
- **SQLite** - Persistent event storage
- **Eventium Memory** - In-memory option
- **Persistent** - Type-safe database access
- **Optparse-Applicative** - CLI parsing
- **Aeson** - JSON serialization
- **Lens** - Functional record updates
- **ANSI Terminal** - Colored terminal output

## Comparison with Bank Example

| Feature | Bank Example | Cafe Example |
|---------|--------------|--------------|
| **Domain** | Financial transactions | Restaurant orders |
| **Aggregates** | Account, Customer | Tab |
| **Process Manager** | ✅ Transfer workflow | ❌ Single aggregate |
| **Read Models** | Customer accounts | Chef todo list |
| **Workflow** | Multi-aggregate | Single aggregate, multi-step |
| **Complexity** | High (transfers, sagas) | Medium (workflow states) |

## Learning Path

1. **Start Simple** - Open tab, place order, close tab
2. **Explore State** - View tab state after each command
3. **Study Events** - See how events build state
4. **Check Read Model** - Watch chef todo list update
5. **Understand Validation** - Try invalid commands (serve unordered items)

## Next Steps

- Try the [Bank Example](../bank/) for more complex patterns
- Check the [Counter CLI](../counter-cli/) for basics
- Read the [Design Documentation](../../DESIGN.md)
- Explore [Main README](../../README.md)

## Credits

Inspired by [Edument's CQRS Tutorial](http://cqrs.nu/tutorial) - an excellent resource for learning CQRS and Event Sourcing.

## License

MIT - see [LICENSE.md](../../LICENSE.md)
