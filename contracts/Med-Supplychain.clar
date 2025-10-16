(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-TEMPERATURE (err u102))
(define-constant ERR-EXPIRED-PRODUCT (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-INVALID-PARTICIPANT (err u106))
(define-constant ERR-INVALID-SCORE (err u107))
(define-constant ERR-INVALID-BATCH-DATA (err u108))
(define-constant ERR-INSUFFICIENT-BATCH-DATA (err u109))

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

(define-map batch-analytics
  { batch-number: (string-ascii 20) }
  {
    total-products: uint,
    products-recalled: uint,
    avg-temp-violations: uint,
    total-transfers: uint,
    failed-transfers: uint,
    avg-delivery-time: uint,
    risk-score: uint,
    risk-level: (string-ascii 10),
    last-updated: uint,
    manufacturer: principal
  }
)

(define-map location-risk-metrics
  { location: (string-ascii 50) }
  {
    total-events: uint,
    temperature-violations: uint,
    successful-transfers: uint,
    failed-transfers: uint,
    avg-risk-score: uint,
    risk-tier: (string-ascii 10)
  }
)

(define-map manufacturer-batch-stats
  { manufacturer: principal }
  {
    total-batches: uint,
    high-risk-batches: uint,
    avg-batch-risk: uint,
    total-recalls: uint,
    manufacturing-quality-score: uint
  }
)

(define-map risk-alerts
  { alert-id: uint }
  {
    batch-number: (string-ascii 20),
    manufacturer: principal,
    risk-type: (string-ascii 20),
    severity: (string-ascii 10),
    alert-message: (string-ascii 100),
    timestamp: uint,
    acknowledged: bool
  }
)

(define-data-var alert-counter uint u0)

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

(define-private (initialize-batch-analytics (batch-number (string-ascii 20)) (manufacturer principal))
  (if (is-none (map-get? batch-analytics { batch-number: batch-number }))
    (map-set batch-analytics
      { batch-number: batch-number }
      {
        total-products: u0,
        products-recalled: u0,
        avg-temp-violations: u0,
        total-transfers: u0,
        failed-transfers: u0,
        avg-delivery-time: u0,
        risk-score: u100,
        risk-level: "low",
        last-updated: stacks-block-height,
        manufacturer: manufacturer
      }
    )
    false
  )
)

(define-private (calculate-risk-level (risk-score uint))
  (if (>= risk-score u800)
    "critical"
    (if (>= risk-score u600)
      "high"
      (if (>= risk-score u400)
        "medium"
        (if (>= risk-score u200)
          "low"
          "minimal"
        )
      )
    )
  )
)

(define-private (update-batch-risk-score (batch-number (string-ascii 20)) (risk-adjustment int) (event-type (string-ascii 20)))
  (let 
    (
      (current-analytics (default-to 
        { 
          total-products: u0, 
          products-recalled: u0, 
          avg-temp-violations: u0, 
          total-transfers: u0, 
          failed-transfers: u0, 
          avg-delivery-time: u0, 
          risk-score: u100, 
          risk-level: "low", 
          last-updated: u0, 
          manufacturer: tx-sender 
        } 
        (map-get? batch-analytics { batch-number: batch-number })
      ))
      (new-risk-score (+ (to-int (get risk-score current-analytics)) risk-adjustment))
      (final-risk-score (if (< new-risk-score 0) u0 (to-uint new-risk-score)))
      (new-risk-level (calculate-risk-level final-risk-score))
    )
    (map-set batch-analytics
      { batch-number: batch-number }
      (merge current-analytics 
        { 
          risk-score: final-risk-score,
          risk-level: new-risk-level,
          last-updated: stacks-block-height
        }
      )
    )
    (if (is-eq new-risk-level "critical")
      (begin
        (create-risk-alert batch-number (get manufacturer current-analytics) event-type "critical" "batch-risk-critical")
        true
      )
      (if (is-eq new-risk-level "high")
        (begin
          (create-risk-alert batch-number (get manufacturer current-analytics) event-type "high" "batch-risk-elevated")
          true
        )
        true
      )
    )
    final-risk-score
  )
)

(define-private (create-risk-alert (batch-number (string-ascii 20)) (manufacturer principal) (risk-type (string-ascii 20)) (severity (string-ascii 10)) (message (string-ascii 100)))
  (let ((alert-id (+ (var-get alert-counter) u1)))
    (var-set alert-counter alert-id)
    (map-set risk-alerts
      { alert-id: alert-id }
      {
        batch-number: batch-number,
        manufacturer: manufacturer,
        risk-type: risk-type,
        severity: severity,
        alert-message: message,
        timestamp: stacks-block-height,
        acknowledged: false
      }
    )
    alert-id
  )
)

(define-private (update-location-risk-metrics (location (string-ascii 50)) (event-type (string-ascii 20)) (success bool))
  (let 
    (
      (current-metrics (default-to 
        { 
          total-events: u0, 
          temperature-violations: u0, 
          successful-transfers: u0, 
          failed-transfers: u0, 
          avg-risk-score: u100, 
          risk-tier: "low" 
        } 
        (map-get? location-risk-metrics { location: location })
      ))
      (updated-metrics
        (if success
          (merge current-metrics 
            { 
              total-events: (+ (get total-events current-metrics) u1),
              successful-transfers: (+ (get successful-transfers current-metrics) u1)
            }
          )
          (merge current-metrics 
            { 
              total-events: (+ (get total-events current-metrics) u1),
              failed-transfers: (+ (get failed-transfers current-metrics) u1)
            }
          )
        )
      )
    )
    (map-set location-risk-metrics { location: location } updated-metrics)
    true
  )
)

(define-private (update-manufacturer-stats (manufacturer principal) (batch-number (string-ascii 20)) (risk-score uint))
  (let 
    (
      (current-stats (default-to 
        { 
          total-batches: u0, 
          high-risk-batches: u0, 
          avg-batch-risk: u100, 
          total-recalls: u0, 
          manufacturing-quality-score: u1000 
        } 
        (map-get? manufacturer-batch-stats { manufacturer: manufacturer })
      ))
      (is-high-risk (>= risk-score u600))
      (updated-stats
        (merge current-stats 
          { 
            total-batches: (+ (get total-batches current-stats) u1),
            high-risk-batches: (if is-high-risk (+ (get high-risk-batches current-stats) u1) (get high-risk-batches current-stats)),
            avg-batch-risk: (/ (+ (* (get avg-batch-risk current-stats) (get total-batches current-stats)) risk-score) (+ (get total-batches current-stats) u1))
          }
        )
      )
    )
    (map-set manufacturer-batch-stats { manufacturer: manufacturer } updated-stats)
    true
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
      (initialize-batch-analytics batch-number tx-sender)
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
          (update-batch-risk-score (get batch-number product) 25 "transfer-success")
          (update-location-risk-metrics location "transfer" true)
          (ok true)
        )
        (begin
          (record-quality-event tx-sender "temp-violation" -50 product-id "temperature-out-of-range")
          (update-batch-risk-score (get batch-number product) -100 "temp-violation")
          (update-location-risk-metrics location "temp-violation" false)
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

(define-public (acknowledge-risk-alert (alert-id uint))
  (let ((alert (unwrap! (map-get? risk-alerts { alert-id: alert-id }) ERR-PRODUCT-NOT-FOUND)))
    (if (or (is-contract-owner) (is-eq tx-sender (get manufacturer alert)))
      (begin
        (map-set risk-alerts
          { alert-id: alert-id }
          (merge alert { acknowledged: true })
        )
        (ok true)
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

(define-public (manual-batch-risk-adjustment (batch-number (string-ascii 20)) (risk-adjustment int) (reason (string-ascii 100)))
  (if (is-contract-owner)
    (let ((final-risk (update-batch-risk-score batch-number risk-adjustment "manual-adjustment")))
      (ok final-risk)
    )
    ERR-NOT-AUTHORIZED
  )
)

(define-read-only (get-batch-analytics (batch-number (string-ascii 20)))
  (map-get? batch-analytics { batch-number: batch-number })
)

(define-read-only (get-batch-risk-score (batch-number (string-ascii 20)))
  (match (map-get? batch-analytics { batch-number: batch-number })
    analytics (get risk-score analytics)
    u0
  )
)

(define-read-only (get-batch-risk-level (batch-number (string-ascii 20)))
  (match (map-get? batch-analytics { batch-number: batch-number })
    analytics (get risk-level analytics)
    "unknown"
  )
)

(define-read-only (get-location-risk-metrics (location (string-ascii 50)))
  (map-get? location-risk-metrics { location: location })
)

(define-read-only (get-manufacturer-batch-stats (manufacturer principal))
  (map-get? manufacturer-batch-stats { manufacturer: manufacturer })
)

(define-read-only (get-risk-alert (alert-id uint))
  (map-get? risk-alerts { alert-id: alert-id })
)

(define-read-only (get-active-risk-alerts)
  (fold filter-active-alerts (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) (list))
)

(define-private (filter-active-alerts (alert-id uint) (acc (list 10 { alert-id: uint, batch-number: (string-ascii 20), severity: (string-ascii 10) })))
  (match (map-get? risk-alerts { alert-id: alert-id })
    alert 
      (if (not (get acknowledged alert))
        (unwrap-panic (as-max-len? (append acc { alert-id: alert-id, batch-number: (get batch-number alert), severity: (get severity alert) }) u10))
        acc
      )
    acc
  )
)

(define-read-only (get-high-risk-batches)
  (fold filter-high-risk-batches (list "BATCH-001" "BATCH-002" "BATCH-003" "BATCH-004" "BATCH-005") (list))
)

(define-private (filter-high-risk-batches (batch-number (string-ascii 20)) (acc (list 5 { batch: (string-ascii 20), risk-score: uint, risk-level: (string-ascii 10) })))
  (match (map-get? batch-analytics { batch-number: batch-number })
    analytics 
      (if (>= (get risk-score analytics) u600)
        (unwrap-panic (as-max-len? (append acc { batch: batch-number, risk-score: (get risk-score analytics), risk-level: (get risk-level analytics) }) u5))
        acc
      )
    acc
  )
)

(define-read-only (analyze-manufacturer-risk (manufacturer principal))
  (match (map-get? manufacturer-batch-stats { manufacturer: manufacturer })
    stats 
      {
        manufacturer: manufacturer,
        risk-assessment: (if (> (get avg-batch-risk stats) u600) "high-risk" (if (> (get avg-batch-risk stats) u400) "medium-risk" "low-risk")),
        total-batches: (get total-batches stats),
        high-risk-percentage: (if (> (get total-batches stats) u0) (/ (* (get high-risk-batches stats) u100) (get total-batches stats)) u0),
        manufacturing-quality: (get manufacturing-quality-score stats),
        total-recalls: (get total-recalls stats)
      }
    { 
      manufacturer: manufacturer, 
      risk-assessment: "unknown", 
      total-batches: u0, 
      high-risk-percentage: u0, 
      manufacturing-quality: u0, 
      total-recalls: u0 
    }
  )
)

(define-read-only (get-location-risk-assessment (location (string-ascii 50)))
  (match (map-get? location-risk-metrics { location: location })
    metrics 
      (let 
        (
          (success-rate (if (> (get total-events metrics) u0) (/ (* (get successful-transfers metrics) u100) (get total-events metrics)) u0))
          (violation-rate (if (> (get total-events metrics) u0) (/ (* (get temperature-violations metrics) u100) (get total-events metrics)) u0))
        )
        {
          location: location,
          risk-tier: (if (< success-rate u70) "high-risk" (if (< success-rate u85) "medium-risk" "low-risk")),
          success-rate: success-rate,
          violation-rate: violation-rate,
          total-events: (get total-events metrics),
          avg-risk-score: (get avg-risk-score metrics)
        }
      )
    { 
      location: location, 
      risk-tier: "unknown", 
      success-rate: u0, 
      violation-rate: u0, 
      total-events: u0, 
      avg-risk-score: u0 
    }
  )
)

(define-read-only (get-supply-chain-risk-overview)
  (let 
    (
      (sample-batches (list "BATCH-001" "BATCH-002" "BATCH-003"))
      (risk-scores (map get-batch-risk-score sample-batches))
      (total-risk-score (fold + risk-scores u0))
      (batch-count (len sample-batches))
    )
    {
      total-batches-analyzed: batch-count,
      average-risk-score: (if (> batch-count u0) (/ total-risk-score batch-count) u0),
      total-alerts: (var-get alert-counter),
      high-risk-batch-count: (len (get-high-risk-batches)),
      system-risk-level: (if (> (/ total-risk-score batch-count) u600) "high" "normal")
    }
  )
)

(define-read-only (predict-batch-risk (batch-number (string-ascii 20)) (manufacturer principal) (planned-locations (list 3 (string-ascii 50))))
  (let 
    (
      (manufacturer-stats (get total-recalls (default-to { total-batches: u0, high-risk-batches: u0, avg-batch-risk: u100, total-recalls: u0, manufacturing-quality-score: u1000 } (map-get? manufacturer-batch-stats { manufacturer: manufacturer }))))
      (location-risks (map get-location-risk-score planned-locations))
      (avg-location-risk (if (> (len planned-locations) u0) (/ (fold + location-risks u0) (len planned-locations)) u100))
      (base-risk u200)
      (predicted-risk (+ base-risk (* manufacturer-stats u10) (/ avg-location-risk u2)))
    )
    {
      batch-number: batch-number,
      predicted-risk-score: predicted-risk,
      predicted-risk-level: (calculate-risk-level predicted-risk),
      manufacturer-risk-factor: manufacturer-stats,
      location-risk-factor: avg-location-risk,
      recommendation: (if (> predicted-risk u600) "review-before-shipping" "proceed-with-monitoring")
    }
  )
)

(define-private (get-location-risk-score (location (string-ascii 50)))
  (match (map-get? location-risk-metrics { location: location })
    metrics (get avg-risk-score metrics)
    u100
  )
)

(define-read-only (get-critical-risk-alerts)
  (fold filter-critical-alerts (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) (list))
)

(define-private (filter-critical-alerts (alert-id uint) (acc (list 10 { alert-id: uint, batch: (string-ascii 20), message: (string-ascii 100), timestamp: uint })))
  (match (map-get? risk-alerts { alert-id: alert-id })
    alert 
      (if (and (is-eq (get severity alert) "critical") (not (get acknowledged alert)))
        (unwrap-panic (as-max-len? (append acc { alert-id: alert-id, batch: (get batch-number alert), message: (get alert-message alert), timestamp: (get timestamp alert) }) u10))
        acc
      )
    acc
  )
)

(define-read-only (get-batch-performance-trends (batch-number (string-ascii 20)))
  (match (map-get? batch-analytics { batch-number: batch-number })
    analytics 
      (let 
        (
          (risk-trend (if (> (get risk-score analytics) u400) "deteriorating" "stable"))
          (transfer-success-rate (if (> (get total-transfers analytics) u0) (/ (* (- (get total-transfers analytics) (get failed-transfers analytics)) u100) (get total-transfers analytics)) u100))
        )
        {
          batch-number: batch-number,
          current-risk: (get risk-score analytics),
          risk-trend: risk-trend,
          transfer-success-rate: transfer-success-rate,
          temperature-violations: (get avg-temp-violations analytics),
          total-products: (get total-products analytics),
          recall-status: (> (get products-recalled analytics) u0),
          last-updated: (get last-updated analytics)
        }
      )
    { 
      batch-number: batch-number, 
      current-risk: u0, 
      risk-trend: "unknown", 
      transfer-success-rate: u0, 
      temperature-violations: u0, 
      total-products: u0, 
      recall-status: false, 
      last-updated: u0 
    }
  )
)

(define-data-var next-insurance-policy-id uint u1)
(define-data-var next-insurance-claim-id uint u1)
(define-data-var total-insurance-premiums uint u0)
(define-data-var total-insurance-payouts uint u0)

(define-map insurance-policies
  { policy-id: uint }
  {
    policy-holder: principal,
    product-id: uint,
    batch-number: (optional (string-ascii 20)),
    coverage-amount: uint,
    premium-paid: uint,
    policy-start: uint,
    policy-end: uint,
    risk-score: uint,
    reputation-score: uint,
    is-active: bool
  }
)

(define-map policy-lookup
  { product-id: uint, batch-number: (optional (string-ascii 20)) }
  { policy-id: uint, is-active: bool }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    event-type: uint,
    claim-amount: uint,
    is-paid: bool,
    reporter: principal,
    timestamp: uint,
    reason: (optional (string-ascii 64))
  }
)

(define-read-only (calculate-insurance-premium (coverage-amount uint) (risk-score uint) (reputation-score uint) (duration-blocks uint))
  (if (or (is-eq coverage-amount u0) (is-eq duration-blocks u0) (> risk-score u100) (> reputation-score u100))
    u0
    (let 
      (
        (duration-factor (if (> duration-blocks u100) u100 duration-blocks))
        (risk-factor (if (> (+ risk-score (- u100 reputation-score)) u5) (+ risk-score (- u100 reputation-score)) u5))
        (base-premium (/ (* coverage-amount risk-factor) u1000))
        (adjusted-premium (/ (* base-premium duration-factor) u100))
        (minimum-premium u10000)
      )
      (if (< adjusted-premium minimum-premium) minimum-premium adjusted-premium)
    )
  )
)

(define-public (create-insurance-policy 
  (product-id uint) 
  (batch-number (optional (string-ascii 20))) 
  (coverage-amount uint) 
  (duration-blocks uint))
  (let 
    (
      (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
      (participant-metrics (default-to 
        { quality-score: u1000, reputation-tier: "new" } 
        (map-get? participant-quality-metrics { participant: tx-sender })))
      (batch-risk (if (is-some batch-number) 
                    (get-batch-risk-score (unwrap-panic batch-number)) 
                    u100))
      (risk-score (/ (+ batch-risk u100) u2))
      (reputation-score (/ (get quality-score participant-metrics) u10))
      (premium (calculate-insurance-premium coverage-amount risk-score reputation-score duration-blocks))
      (existing-policy (default-to { policy-id: u0, is-active: false } 
                        (map-get? policy-lookup { product-id: product-id, batch-number: batch-number })))
    )
    (if (or (is-eq premium u0) (get is-active existing-policy))
      (err u400)
      (let 
        (
          (policy-start stacks-block-height)
          (policy-end (+ stacks-block-height duration-blocks))
          (new-policy-id (var-get next-insurance-policy-id))
        )
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        (map-set insurance-policies
          { policy-id: new-policy-id }
          {
            policy-holder: tx-sender,
            product-id: product-id,
            batch-number: batch-number,
            coverage-amount: coverage-amount,
            premium-paid: premium,
            policy-start: policy-start,
            policy-end: policy-end,
            risk-score: risk-score,
            reputation-score: reputation-score,
            is-active: true
          }
        )
        (map-set policy-lookup
          { product-id: product-id, batch-number: batch-number }
          { policy-id: new-policy-id, is-active: true }
        )
        (var-set next-insurance-policy-id (+ new-policy-id u1))
        (var-set total-insurance-premiums (+ (var-get total-insurance-premiums) premium))
        (ok { policy-id: new-policy-id, premium-paid: premium })
      )
    )
  )
)

(define-public (file-insurance-claim 
  (product-id uint) 
  (batch-number (optional (string-ascii 20))) 
  (event-type uint) 
  (reason (optional (string-ascii 64))))
  (let 
    (
      (policy-lookup-result (map-get? policy-lookup { product-id: product-id, batch-number: batch-number }))
    )
    (if (is-none policy-lookup-result)
      (err u404)
      (let 
        (
          (lookup-data (unwrap! policy-lookup-result (err u404)))
          (policy-id (get policy-id lookup-data))
          (policy-data (unwrap! (map-get? insurance-policies { policy-id: policy-id }) (err u404)))
        )
        (if (or 
              (not (get is-active policy-data)) 
              (< stacks-block-height (get policy-start policy-data)) 
              (> stacks-block-height (get policy-end policy-data)))
          (err u405)
          (let 
            (
              (event-severity (if (is-eq event-type u1) u20 (if (is-eq event-type u2) u10 u15)))
              (risk-adjustment (/ (+ (get risk-score policy-data) (- u100 (get reputation-score policy-data))) u2))
              (total-factor (+ event-severity risk-adjustment))
              (payout-rate (if (> total-factor u100) u100 total-factor))
              (claim-amount (/ (* (get coverage-amount policy-data) payout-rate) u100))
              (available-balance (as-contract (stx-get-balance tx-sender)))
              (new-claim-id (var-get next-insurance-claim-id))
            )
            (if (or (is-eq claim-amount u0) (> claim-amount available-balance))
              (err u406)
              (begin
                (map-set insurance-claims
                  { claim-id: new-claim-id }
                  {
                    policy-id: policy-id,
                    event-type: event-type,
                    claim-amount: claim-amount,
                    is-paid: true,
                    reporter: tx-sender,
                    timestamp: stacks-block-height,
                    reason: reason
                  }
                )
                (var-set next-insurance-claim-id (+ new-claim-id u1))
                (var-set total-insurance-payouts (+ (var-get total-insurance-payouts) claim-amount))
                (map-set policy-lookup
                  { product-id: product-id, batch-number: batch-number }
                  { policy-id: policy-id, is-active: false }
                )
                (map-set insurance-policies
                  { policy-id: policy-id }
                  (merge policy-data { is-active: false })
                )
                (try! (as-contract (stx-transfer? claim-amount tx-sender (get policy-holder policy-data))))
                (ok new-claim-id)
              )
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-insurance-stats)
  {
    total-policies: (var-get next-insurance-policy-id),
    total-claims: (var-get next-insurance-claim-id),
    total-premiums: (var-get total-insurance-premiums),
    total-payouts: (var-get total-insurance-payouts),
    available-reserves: (as-contract (stx-get-balance tx-sender))
  }
)
