# Medical Supply Chain Transparency

A blockchain-based solution for tracking pharmaceuticals from manufacturer to patient, ensuring authenticity, preventing counterfeiting, and maintaining proper storage conditions throughout the supply chain.

## Features

- **Product Registration**: Manufacturers can register pharmaceutical products with batch details and temperature requirements
- **Supply Chain Tracking**: Complete tracking of product movement through the supply chain
- **Temperature Monitoring**: Real-time temperature logging to ensure cold chain compliance
- **Authentication**: Role-based access control for authorized participants
- **Anti-Counterfeiting**: Immutable blockchain records prevent tampering and counterfeiting
- **Emergency Recall**: Manufacturers can issue emergency recalls for products
- **Expiry Monitoring**: Automatic expiry date checking to prevent dispensing expired medications

## Smart Contract Functions

### Public Functions

#### Product Management
- `register-product` - Register a new pharmaceutical product
- `transfer-product` - Transfer product to another authorized party
- `receive-product` - Confirm receipt of a product
- `dispense-to-patient` - Final dispensing to patient
- `update-product-status` - Update product status

#### Temperature & Monitoring
- `log-temperature` - Record temperature readings
- `emergency-recall` - Issue emergency product recall

#### Access Control
- `authorize-participant` - Authorize new supply chain participants (contract owner only)
- `revoke-participant` - Revoke participant authorization (contract owner only)

#### Batch Operations
- `batch-transfer-products` - Transfer multiple products at once

### Read-Only Functions

#### Product Information
- `get-product` - Get complete product information
- `get-product-count` - Get total number of registered products
- `verify-product-authenticity` - Verify if a product is authentic
- `is-product-expired` - Check if product has expired
- `get-product-chain-summary` - Get summary of product's supply chain journey

#### Supply Chain History
- `get-supply-chain-event` - Get specific supply chain event
- `get-product-history` - Get complete history of product events
- `validate-supply-chain` - Validate integrity of supply chain

#### Temperature & Compliance
- `get-temperature-log` - Get specific temperature log entry
- `check-temperature-compliance` - Check if product maintained proper temperature

#### Filtering & Search
- `get-products-by-manufacturer` - Get all products from a specific manufacturer
- `get-products-by-status` - Get all products with specific status
- `get-expired-products` - Get all expired products
- `get-recall-status` - Check if product is recalled

#### Participant Management
- `get-participant-info` - Get participant role and authorization status

## Usage Example

### 1. Authorize Participants

```clarity
;; Contract owner authorizes a distributor
(contract-call? .Med-Supplychain authorize-participant 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 "distributor")

;; Authorize a pharmacy
(contract-call? .Med-Supplychain authorize-participant 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE "pharmacy")
```

### 2. Register Product

```clarity
;; Manufacturer registers a new pharmaceutical product
(contract-call? .Med-Supplychain register-product 
  "Amoxicillin-500mg" 
  "BATCH-2024-001" 
  u1000 
  u2000 
  2 
  8)
```

### 3. Track Supply Chain

```clarity
;; Transfer to distributor
(contract-call? .Med-Supplychain transfer-product 
  u1 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 
  "Distribution-Center-NYC" 
  5 
  "shipped-via-fedex")

;; Log temperature during transport
(contract-call? .Med-Supplychain log-temperature 
  u1 
  4 
  "Transport-Vehicle-001")

;; Receive at pharmacy
(contract-call? .Med-Supplychain receive-product 
  u1 
  "Pharmacy-Downtown" 
  6 
  "good-condition")
```

### 4. Dispense to Patient

```clarity
;; Final dispensing to patient
(contract-call? .Med-Supplychain dispense-to-patient 
  u1 
  "PATIENT-ID-12345" 
  "Pharmacy-Downtown" 
  7)
```

### 5. Verify Product

```clarity
;; Check product authenticity
(contract-call? .Med-Supplychain verify-product-authenticity u1)

;; Get complete supply chain history
(contract-call? .Med-Supplychain get-product-chain-summary u1)
```

## Data Structures

### Product
- `manufacturer`: Principal who manufactured the product
- `name`: Product name (max 50 characters)
- `batch-number`: Manufacturing batch identifier
- `manufacturing-date`: Block height when manufactured
- `expiry-date`: Block height when product expires
- `min-temp`/`max-temp`: Required temperature range in Celsius
- `current-status`: Current status in supply chain
- `current-holder`: Current holder of the product

### Supply Chain Event
- `event-type`: Type of event (manufactured, transferred, received, dispensed, recalled)
- `from-party`/`to-party`: Parties involved in the event
- `timestamp`: Block height when event occurred
- `location`: Physical location of the event
- `temperature`: Temperature at time of event
- `notes`: Additional event details

### Temperature Log
- `temperature`: Recorded temperature
- `timestamp`: Block height when recorded
- `recorded-by`: Principal who recorded the temperature
- `location`: Location where temperature was recorded

## Error Codes

- `u100`: Not authorized
- `u101`: Product not found
- `u102`: Invalid temperature (outside required range)
- `u103`: Expired product
- `u104`: Already exists
- `u105`: Invalid status
- `u106`: Invalid participant

## Development

This project uses Clarinet for smart contract development and testing.

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet)
- [Stacks CLI](https://docs.stacks.co/references/stacks-cli)

### Testing
```bash
clarinet test
```

### Console
```bash
clarinet console
```

## Security Features

- **Immutable Records**: All transactions are permanently recorded on the blockchain
- **Role-Based Access**: Only authorized participants can perform specific actions
- **Temperature Validation**: Automatic validation of temperature requirements
- **Expiry Checking**: Prevents dispensing of expired medications
- **Emergency Recalls**: Immediate recall capability for safety issues
- **Authenticity Verification**: Built-in verification against counterfeiting

## License

MIT License
