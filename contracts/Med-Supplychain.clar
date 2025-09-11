(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-TEMPERATURE (err u102))
(define-constant ERR-EXPIRED-PRODUCT (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-INVALID-PARTICIPANT (err u106))
(define-constant ERR-INVALID-SCORE (err u107))

(define-data-var contract-owner principal tx-sender)
(define-data-var product-counter uint u0)
(define-data-var shipment-counter uint u0)

(define-map products
  { product-id: uint }
  {
    manufacturer: principal,
    name: (string-ascii 50),
    batch-number: (string-ascii 20),
    manufacturing-date: uint,
    expiry-date: uint,
    min-temp: int,
    max-temp: int,
    current-status: (string-ascii 20),
    current-holder: principal,
    created-at: uint
  }
)

(define-map authorized-participants
  { participant: principal }
  { role: (string-ascii 20), authorized: bool }
)

(define-map supply-chain-events
  { product-id: uint, event-id: uint }
  {
    event-type: (string-ascii 20),
    from-party: principal,
    to-party: principal,
    timestamp: uint,
    location: (string-ascii 50),
    temperature: int,
    notes: (string-ascii 100)
  }
)

(define-map temperature-logs
  { product-id: uint, log-id: uint }
  {
    temperature: int,
    timestamp: uint,
    recorded-by: principal,
    location: (string-ascii 50)
  }
)

(define-map product-event-counts
  { product-id: uint }
  { count: uint }
)

(define-map product-temp-counts
  { product-id: uint }
  { count: uint }
)

(define-map participant-quality-metrics
  { participant: principal }
  {
    total-transactions: uint,
    successful-transfers: uint,
    temperature-violations: uint,
    on-time-deliveries: uint,
    late-deliveries: uint,
    quality-score: uint,
    last-updated: uint,
    reputation-tier: (string-ascii 10)
  }
)

(define-map quality-events
  { participant: principal, event-id: uint }
  {
    event-type: (string-ascii 20),
    score-impact: int,
    timestamp: uint,
    product-id: uint,
    details: (string-ascii 100)
  }
)

(define-map participant-quality-event-counts
  { participant: principal }
  { count: uint }
)

(define-private (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-authorized (participant principal))
  (default-to false 
    (get authorized (map-get? authorized-participants { participant: participant }))
  )
)

(define-private (is-manufacturer (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (is-eq tx-sender (get manufacturer product))
    false
  )
)

(define-private (is-current-holder (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (is-eq tx-sender (get current-holder product))
    false
  )
)

(define-private (increment-product-counter)
  (let ((current-count (var-get product-counter)))
    (var-set product-counter (+ current-count u1))
    current-count
  )
)

(define-private (get-next-event-id (product-id uint))
  (let ((current-count (default-to u0 (get count (map-get? product-event-counts { product-id: product-id })))))
    (map-set product-event-counts { product-id: product-id } { count: (+ current-count u1) })
    current-count
  )
)

(define-private (get-next-temp-log-id (product-id uint))
  (let ((current-count (default-to u0 (get count (map-get? product-temp-counts { product-id: product-id })))))
    (map-set product-temp-counts { product-id: product-id } { count: (+ current-count u1) })
    current-count
  )
)

(define-private (get-next-quality-event-id (participant principal))
  (let ((current-count (default-to u0 (get count (map-get? participant-quality-event-counts { participant: participant })))))
    (map-set participant-quality-event-counts { participant: participant } { count: (+ current-count u1) })
    current-count
  )
)

(define-private (initialize-participant-quality (participant principal))
  (if (is-none (map-get? participant-quality-metrics { participant: participant }))
    (map-set participant-quality-metrics
      { participant: participant }
      {
        total-transactions: u0,
        successful-transfers: u0,
        temperature-violations: u0,
        on-time-deliveries: u0,
        late-deliveries: u0,
        quality-score: u1000,
        last-updated: stacks-block-height,
        reputation-tier: "new"
      }
    )
    false
  )
)

(define-private (calculate-reputation-tier (score uint))
  (if (>= score u1500)
    "platinum"
    (if (>= score u1200)
      "gold"
      (if (>= score u800)
        "silver"
        (if (>= score u500)
          "bronze"
          "poor"
        )
      )
    )
  )
)

(define-private (record-quality-event (participant principal) (event-type (string-ascii 20)) (score-impact int) (product-id uint) (details (string-ascii 100)))
  (let 
    (
      (event-id (get-next-quality-event-id participant))
      (current-metrics (default-to 
        { 
          total-transactions: u0, 
          successful-transfers: u0, 
          temperature-violations: u0, 
          on-time-deliveries: u0, 
          late-deliveries: u0, 
          quality-score: u1000, 
          last-updated: u0, 
          reputation-tier: "new" 
        } 
        (map-get? participant-quality-metrics { participant: participant })
      ))
      (new-score (+ (to-int (get quality-score current-metrics)) score-impact))
      (final-score (if (< new-score 0) u0 (to-uint new-score)))
    )
    (map-set quality-events
      { participant: participant, event-id: event-id }
      {
        event-type: event-type,
        score-impact: score-impact,
        timestamp: stacks-block-height,
        product-id: product-id,
        details: details
      }
    )
    (map-set participant-quality-metrics
      { participant: participant }
      (merge current-metrics 
        { 
          quality-score: final-score,
          last-updated: stacks-block-height,
          reputation-tier: (calculate-reputation-tier final-score)
        }
      )
    )
    final-score
  )
)

(define-public (authorize-participant (participant principal) (role (string-ascii 20)))
  (if (is-contract-owner)
    (begin
      (map-set authorized-participants
        { participant: participant }
        { role: role, authorized: true }
      )
      (initialize-participant-quality participant)
      (ok true)
    )
    ERR-NOT-AUTHORIZED
  )
)

(define-public (revoke-participant (participant principal))
  (if (is-contract-owner)
    (begin
      (map-set authorized-participants
        { participant: participant }
        { role: "", authorized: false }
      )
      (ok true)
    )
    ERR-NOT-AUTHORIZED
  )
)

(define-public (register-product 
  (name (string-ascii 50))
  (batch-number (string-ascii 20))
  (manufacturing-date uint)
  (expiry-date uint)
  (min-temp int)
  (max-temp int)
)
  (if (is-authorized tx-sender)
    (let ((product-id (+ (var-get product-counter) u1)))
      (var-set product-counter product-id)
      (map-set products
        { product-id: product-id }
        {
          manufacturer: tx-sender,
          name: name,
          batch-number: batch-number,
          manufacturing-date: manufacturing-date,
          expiry-date: expiry-date,
          min-temp: min-temp,
          max-temp: max-temp,
          current-status: "manufactured",
          current-holder: tx-sender,
          created-at: stacks-block-height
        }
      )
      (map-set supply-chain-events
        { product-id: product-id, event-id: u0 }
        {
          event-type: "manufactured",
          from-party: tx-sender,
          to-party: tx-sender,
          timestamp: stacks-block-height,
          location: "manufacturing-facility",
          temperature: min-temp,
          notes: "product-manufactured"
        }
      )
      (map-set product-event-counts { product-id: product-id } { count: u1 })
      (ok product-id)
    )
    ERR-NOT-AUTHORIZED
  )
)

(define-public (transfer-product 
  (product-id uint)
  (to-party principal)
  (location (string-ascii 50))
  (temperature int)
  (notes (string-ascii 100))
)
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (and (is-current-holder product-id) (is-authorized to-party))
      (if (and (>= temperature (get min-temp product)) (<= temperature (get max-temp product)))
        (let ((event-id (get-next-event-id product-id)))
          (map-set products
            { product-id: product-id }
            (merge product { current-holder: to-party, current-status: "in-transit" })
          )
          (map-set supply-chain-events
            { product-id: product-id, event-id: event-id }
            {
              event-type: "transferred",
              from-party: tx-sender,
              to-party: to-party,
              timestamp: stacks-block-height,
              location: location,
              temperature: temperature,
              notes: notes
            }
          )
          (record-quality-event tx-sender "transfer" 20 product-id "successful-transfer")
          (record-quality-event to-party "received" 10 product-id "product-received")
          (ok true)
        )
        (begin
          (record-quality-event tx-sender "temp-violation" -50 product-id "temperature-out-of-range")
          ERR-INVALID-TEMPERATURE
        )
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-public (receive-product 
  (product-id uint)
  (location (string-ascii 50))
  (temperature int)
  (condition (string-ascii 100))
)
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (is-current-holder product-id)
      (if (and (>= temperature (get min-temp product)) (<= temperature (get max-temp product)))
        (let ((event-id (get-next-event-id product-id)))
          (map-set products
            { product-id: product-id }
            (merge product { current-status: "received" })
          )
          (map-set supply-chain-events
            { product-id: product-id, event-id: event-id }
            {
              event-type: "received",
              from-party: tx-sender,
              to-party: tx-sender,
              timestamp: stacks-block-height,
              location: location,
              temperature: temperature,
              notes: condition
            }
          )
          (record-quality-event tx-sender "receive" 15 product-id "product-received-successfully")
          (ok true)
        )
        ERR-INVALID-TEMPERATURE
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-public (dispense-to-patient 
  (product-id uint)
  (patient-id (string-ascii 30))
  (location (string-ascii 50))
  (temperature int)
)
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (is-current-holder product-id)
      (if (< stacks-block-height (get expiry-date product))
        (if (and (>= temperature (get min-temp product)) (<= temperature (get max-temp product)))
          (let ((event-id (get-next-event-id product-id)))
            (map-set products
              { product-id: product-id }
              (merge product { current-status: "dispensed" })
            )
            (map-set supply-chain-events
              { product-id: product-id, event-id: event-id }
              {
                event-type: "dispensed",
                from-party: tx-sender,
                to-party: tx-sender,
                timestamp: stacks-block-height,
                location: location,
                temperature: temperature,
                notes: patient-id
              }
            )
            (ok true)
          )
          ERR-INVALID-TEMPERATURE
        )
        ERR-EXPIRED-PRODUCT
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-public (log-temperature 
  (product-id uint)
  (temperature int)
  (location (string-ascii 50))
)
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (is-authorized tx-sender)
      (if (and (>= temperature (get min-temp product)) (<= temperature (get max-temp product)))
        (let ((log-id (get-next-temp-log-id product-id)))
          (map-set temperature-logs
            { product-id: product-id, log-id: log-id }
            {
              temperature: temperature,
              timestamp: stacks-block-height,
              recorded-by: tx-sender,
              location: location
            }
          )
          (ok true)
        )
        ERR-INVALID-TEMPERATURE
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-public (update-product-status 
  (product-id uint)
  (new-status (string-ascii 20))
)
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (is-current-holder product-id)
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { current-status: new-status })
        )
        (ok true)
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-supply-chain-event (product-id uint) (event-id uint))
  (map-get? supply-chain-events { product-id: product-id, event-id: event-id })
)

(define-read-only (get-temperature-log (product-id uint) (log-id uint))
  (map-get? temperature-logs { product-id: product-id, log-id: log-id })
)

(define-read-only (get-participant-info (participant principal))
  (map-get? authorized-participants { participant: participant })
)

(define-read-only (get-product-count)
  (var-get product-counter)
)

(define-read-only (is-product-expired (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (>= stacks-block-height (get expiry-date product))
    true
  )
)

(define-read-only (verify-product-authenticity (product-id uint))
  (match (map-get? products { product-id: product-id })
    product 
      {
        exists: true,
        manufacturer: (get manufacturer product),
        batch: (get batch-number product),
        status: (get current-status product),
        expired: (>= stacks-block-height (get expiry-date product))
      }
    { exists: false, manufacturer: tx-sender, batch: "", status: "", expired: true }
  )
)

(define-read-only (get-product-history (product-id uint))
  (let ((event-count (default-to u0 (get count (map-get? product-event-counts { product-id: product-id })))))
    (map get-supply-chain-event-helper (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

(define-private (get-supply-chain-event-helper (event-id uint))
  (map-get? supply-chain-events { product-id: u0, event-id: event-id })
)

(define-read-only (check-temperature-compliance (product-id uint))
  (match (map-get? products { product-id: product-id })
    product 
      (let ((temp-count (default-to u0 (get count (map-get? product-temp-counts { product-id: product-id })))))
        { 
          compliant: true, 
          min-required: (get min-temp product), 
          max-required: (get max-temp product),
          total-logs: temp-count 
        }
      )
    { compliant: false, min-required: 0, max-required: 0, total-logs: u0 }
  )
)

(define-read-only (get-products-by-manufacturer (manufacturer principal))
  (get results (fold check-manufacturer-fold (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) { manufacturer: manufacturer, results: (list) }))
)

(define-private (check-manufacturer-fold (product-id uint) (acc { manufacturer: principal, results: (list 10 uint) }))
  (if (check-manufacturer-match product-id (get manufacturer acc))
    { manufacturer: (get manufacturer acc), results: (unwrap-panic (as-max-len? (append (get results acc) product-id) u10)) }
    acc
  )
)

(define-private (check-manufacturer-match (product-id uint) (manufacturer principal))
  (match (map-get? products { product-id: product-id })
    product (is-eq (get manufacturer product) manufacturer)
    false
  )
)

(define-read-only (get-products-by-status (status (string-ascii 20)))
  (get results (fold check-status-fold (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) { status: status, results: (list) }))
)

(define-private (check-status-fold (product-id uint) (acc { status: (string-ascii 20), results: (list 10 uint) }))
  (if (check-status-match product-id (get status acc))
    { status: (get status acc), results: (unwrap-panic (as-max-len? (append (get results acc) product-id) u10)) }
    acc
  )
)

(define-private (check-status-match (product-id uint) (status (string-ascii 20)))
  (match (map-get? products { product-id: product-id })
    product (is-eq (get current-status product) status)
    false
  )
)

(define-read-only (get-expired-products)
  (fold check-expired-fold (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) (list))
)

(define-private (check-expired-fold (product-id uint) (acc (list 10 uint)))
  (if (is-product-expired product-id)
    (unwrap-panic (as-max-len? (append acc product-id) u10))
    acc
  )
)

(define-public (batch-transfer-products 
  (product-ids (list 5 uint))
  (to-party principal)
  (location (string-ascii 50))
  (temperature int)
)
  (if (is-authorized to-party)
    (begin
      (fold transfer-product-helper product-ids { to-party: to-party, location: location, temperature: temperature, success: true })
      (ok true)
    )
    ERR-NOT-AUTHORIZED
  )
)

(define-private (transfer-product-helper 
  (product-id uint) 
  (params { to-party: principal, location: (string-ascii 50), temperature: int, success: bool })
)
  (if (get success params)
    (match (transfer-product product-id (get to-party params) (get location params) (get temperature params) "batch-transfer")
      success params
      error (merge params { success: false })
    )
    params
  )
)

(define-read-only (get-product-chain-summary (product-id uint))
  (match (map-get? products { product-id: product-id })
    product 
      (some {
        product-id: product-id,
        name: (get name product),
        manufacturer: (get manufacturer product),
        current-holder: (get current-holder product),
        status: (get current-status product),
        manufacturing-date: (get manufacturing-date product),
        expiry-date: (get expiry-date product),
        expired: (>= stacks-block-height (get expiry-date product)),
        total-events: (default-to u0 (get count (map-get? product-event-counts { product-id: product-id }))),
        temp-logs: (default-to u0 (get count (map-get? product-temp-counts { product-id: product-id })))
      })
    none
  )
)

(define-read-only (validate-supply-chain (product-id uint))
  (match (map-get? products { product-id: product-id })
    product 
      (let 
        (
          (event-count (default-to u0 (get count (map-get? product-event-counts { product-id: product-id }))))
          (temp-count (default-to u0 (get count (map-get? product-temp-counts { product-id: product-id }))))
        )
        {
          valid: (and (> event-count u0) (not (>= stacks-block-height (get expiry-date product)))),
          events-recorded: event-count,
          temperature-logs: temp-count,
          current-status: (get current-status product),
          expired: (>= stacks-block-height (get expiry-date product))
        }
      )
    { valid: false, events-recorded: u0, temperature-logs: u0, current-status: "", expired: true }
  )
)

(define-public (emergency-recall (product-id uint) (reason (string-ascii 100)))
  (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (or (is-contract-owner) (is-manufacturer product-id))
      (let ((event-id (get-next-event-id product-id)))
        (map-set products
          { product-id: product-id }
          (merge product { current-status: "recalled" })
        )
        (map-set supply-chain-events
          { product-id: product-id, event-id: event-id }
          {
            event-type: "recalled",
            from-party: tx-sender,
            to-party: tx-sender,
            timestamp: stacks-block-height,
            location: "system",
            temperature: 0,
            notes: reason
          }
        )
        (ok true)
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-read-only (get-recall-status (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (is-eq (get current-status product) "recalled")
    false
  )
)

(define-public (update-quality-score (participant principal) (score-adjustment int) (reason (string-ascii 100)))
  (if (is-contract-owner)
    (let ((current-score (record-quality-event participant "manual-adjustment" score-adjustment u0 reason)))
      (ok current-score)
    )
    ERR-NOT-AUTHORIZED
  )
)

(define-read-only (get-participant-quality-score (participant principal))
  (match (map-get? participant-quality-metrics { participant: participant })
    metrics (get quality-score metrics)
    u0
  )
)

(define-read-only (get-participant-quality-metrics (participant principal))
  (map-get? participant-quality-metrics { participant: participant })
)

(define-read-only (get-participant-reputation-tier (participant principal))
  (match (map-get? participant-quality-metrics { participant: participant })
    metrics (get reputation-tier metrics)
    "unknown"
  )
)

(define-read-only (get-quality-event (participant principal) (event-id uint))
  (map-get? quality-events { participant: participant, event-id: event-id })
)

(define-read-only (get-top-quality-participants)
  (fold rank-participant-by-quality (list tx-sender (var-get contract-owner)) (list))
)

(define-private (rank-participant-by-quality (participant principal) (acc (list 5 { participant: principal, score: uint })))
  (let ((score (get-participant-quality-score participant)))
    (if (> score u0)
      (unwrap-panic (as-max-len? (append acc { participant: participant, score: score }) u5))
      acc
    )
  )
)

(define-read-only (get-participants-by-tier (tier (string-ascii 10)))
  (get results (fold filter-participants-by-tier (list tx-sender (var-get contract-owner)) { tier: tier, results: (list) }))
)

(define-private (filter-participants-by-tier (participant principal) (acc { tier: (string-ascii 10), results: (list 5 principal) }))
  (if (is-eq (get-participant-reputation-tier participant) (get tier acc))
    { tier: (get tier acc), results: (unwrap-panic (as-max-len? (append (get results acc) participant) u5)) }
    acc
  )
)

(define-read-only (validate-participant-quality (participant principal) (min-score uint))
  (let ((current-score (get-participant-quality-score participant)))
    {
      meets-threshold: (>= current-score min-score),
      current-score: current-score,
      tier: (get-participant-reputation-tier participant),
      authorized: (is-authorized participant)
    }
  )
)

(define-read-only (get-quality-statistics)
  (let 
    (
      (sample-participants (list tx-sender (var-get contract-owner)))
      (scores (map get-participant-quality-score sample-participants))
      (total-score (fold + scores u0))
      (participant-count (len sample-participants))
    )
    {
      total-participants: participant-count,
      average-score: (if (> participant-count u0) (/ total-score participant-count) u0),
      platinum-count: (len (get-participants-by-tier "platinum")),
      gold-count: (len (get-participants-by-tier "gold")),
      silver-count: (len (get-participants-by-tier "silver")),
      bronze-count: (len (get-participants-by-tier "bronze"))
    }
  )
)

(define-read-only (recommend-suppliers (min-score uint) (preferred-tier (string-ascii 10)))
  (get results (fold filter-recommended-suppliers (list tx-sender (var-get contract-owner)) { min-score: min-score, tier: preferred-tier, results: (list) }))
)

(define-private (filter-recommended-suppliers (participant principal) (criteria { min-score: uint, tier: (string-ascii 10), results: (list 4 { participant: principal, score: uint, tier: (string-ascii 10) }) }))
  (let 
    (
      (score (get-participant-quality-score participant))
      (tier (get-participant-reputation-tier participant))
    )
    (if (and (>= score (get min-score criteria)) (is-eq tier (get tier criteria)))
      { 
        min-score: (get min-score criteria), 
        tier: (get tier criteria), 
        results: (unwrap-panic (as-max-len? (append (get results criteria) { participant: participant, score: score, tier: tier }) u4))
      }
      criteria
    )
  )
)
