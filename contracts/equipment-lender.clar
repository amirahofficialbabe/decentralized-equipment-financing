;; Decentralized Equipment Financing Smart Contract
;; Asset-backed lending with IoT monitoring and automated repossession

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-borrower (err u101))
(define-constant err-insufficient-collateral (err u102))
(define-constant err-loan-not-found (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-equipment-not-found (err u106))
(define-constant err-payment-overdue (err u107))
(define-constant err-equipment-inactive (err u108))

;; Data Variables
(define-data-var next-borrower-id uint u1)
(define-data-var next-equipment-id uint u1)
(define-data-var next-loan-id uint u1)
(define-data-var total-loans-originated uint u0)
(define-data-var total-equipment-financed uint u0)
(define-data-var default-interest-rate uint u1200) ;; 12%
(define-data-var repossession-threshold uint u90) ;; 90 days

;; Data Maps
(define-map business-borrowers
    uint
    {
        owner: principal,
        business-name: (string-ascii 100),
        industry: (string-ascii 50),
        credit-score: uint,
        annual-revenue: uint,
        years-in-business: uint,
        verified: bool,
        total-borrowed: uint,
        active-loans: uint,
        payment-history-score: uint
    }
)

(define-map commercial-equipment
    uint
    {
        equipment-type: (string-ascii 100),
        manufacturer: (string-ascii 100),
        model: (string-ascii 100),
        serial-number: (string-ascii 50),
        purchase-price: uint,
        current-value: uint,
        iot-device-id: (string-ascii 100),
        usage-hours: uint,
        condition-score: uint,
        location: (string-ascii 200),
        active: bool,
        last-monitored: uint
    }
)

(define-map equipment-loans
    uint
    {
        borrower-id: uint,
        equipment-id: uint,
        loan-amount: uint,
        interest-rate: uint,
        term-length: uint,
        monthly-payment: uint,
        remaining-balance: uint,
        payments-made: uint,
        last-payment-date: uint,
        next-payment-due: uint,
        loan-status: (string-ascii 20),
        originated-at: uint
    }
)

(define-map iot-sensor-data
    uint
    {
        equipment-id: uint,
        operational-hours: uint,
        revenue-generated: uint,
        maintenance-alerts: uint,
        efficiency-rating: uint,
        geolocation: (string-ascii 100),
        last-activity: uint,
        sensor-status: (string-ascii 20)
    }
)

(define-map loan-payments
    { loan-id: uint, payment-id: uint }
    {
        amount: uint,
        payment-date: uint,
        principal: uint,
        interest: uint,
        late-fee: uint
    }
)

(define-map repossession-orders
    uint
    {
        loan-id: uint,
        equipment-id: uint,
        borrower-id: uint,
        reason: (string-ascii 200),
        initiated-at: uint,
        status: (string-ascii 20),
        recovery-value: uint
    }
)

;; Private Functions
(define-private (calculate-monthly-payment (principal uint) (rate uint) (term uint))
    (let
        (
            (monthly-rate (/ rate u1200))
            (payment-factor (* monthly-rate (pow (+ u1 monthly-rate) term)))
            (denominator (- (pow (+ u1 monthly-rate) term) u1))
        )
        (/ (* principal payment-factor) denominator)
    )
)

(define-private (assess-loan-risk (borrower-id uint) (equipment-value uint))
    (match (map-get? business-borrowers borrower-id)
        borrower
        (let
            (
                (credit (get credit-score borrower))
                (revenue (get annual-revenue borrower))
                (ltv-ratio (/ (* equipment-value u100) revenue))
            )
            (if (and (> credit u650) (< ltv-ratio u30))
                u5   ;; Low risk
                (if (and (> credit u580) (< ltv-ratio u50))
                    u8   ;; Medium risk
                    u12  ;; High risk
                )
            )
        )
        u15  ;; Maximum risk for unknown borrower
    )
)

(define-private (update-equipment-condition (equipment-id uint) (new-condition uint))
    (match (map-get? commercial-equipment equipment-id)
        equipment
        (map-set commercial-equipment equipment-id
            (merge equipment {
                condition-score: new-condition,
                last-monitored: block-height
            })
        )
        false
    )
)

(define-private (check-payment-overdue (loan-id uint))
    (match (map-get? equipment-loans loan-id)
        loan
        (> (- block-height (get next-payment-due loan)) (var-get repossession-threshold))
        false
    )
)

;; Public Functions

;; Register Business Borrower
(define-public (register-business
    (business-name (string-ascii 100))
    (industry (string-ascii 50))
    (annual-revenue uint)
    (years-in-business uint))
    (let
        (
            (borrower-id (var-get next-borrower-id))
        )
        (map-set business-borrowers borrower-id {
            owner: tx-sender,
            business-name: business-name,
            industry: industry,
            credit-score: u650, ;; Default score
            annual-revenue: annual-revenue,
            years-in-business: years-in-business,
            verified: false,
            total-borrowed: u0,
            active-loans: u0,
            payment-history-score: u100
        })
        (var-set next-borrower-id (+ borrower-id u1))
        (ok borrower-id)
    )
)

;; Register Commercial Equipment
(define-public (register-equipment
    (equipment-type (string-ascii 100))
    (manufacturer (string-ascii 100))
    (model (string-ascii 100))
    (serial-number (string-ascii 50))
    (purchase-price uint)
    (iot-device-id (string-ascii 100)))
    (let
        (
            (equipment-id (var-get next-equipment-id))
        )
        (asserts! (> purchase-price u0) err-invalid-amount)
        
        (map-set commercial-equipment equipment-id {
            equipment-type: equipment-type,
            manufacturer: manufacturer,
            model: model,
            serial-number: serial-number,
            purchase-price: purchase-price,
            current-value: purchase-price,
            iot-device-id: iot-device-id,
            usage-hours: u0,
            condition-score: u100,
            location: "warehouse",
            active: true,
            last-monitored: block-height
        })
        
        ;; Initialize IoT monitoring
        (map-set iot-sensor-data equipment-id {
            equipment-id: equipment-id,
            operational-hours: u0,
            revenue-generated: u0,
            maintenance-alerts: u0,
            efficiency-rating: u100,
            geolocation: "0,0",
            last-activity: block-height,
            sensor-status: "active"
        })
        
        (var-set next-equipment-id (+ equipment-id u1))
        (ok equipment-id)
    )
)

;; Apply for Equipment Loan
(define-public (apply-for-loan
    (borrower-id uint)
    (equipment-id uint)
    (loan-amount uint)
    (term-length uint))
    (let
        (
            (loan-id (var-get next-loan-id))
            (borrower (unwrap! (map-get? business-borrowers borrower-id) err-invalid-borrower))
            (equipment (unwrap! (map-get? commercial-equipment equipment-id) err-equipment-not-found))
            (risk-premium (assess-loan-risk borrower-id (get current-value equipment)))
            (interest-rate (+ (var-get default-interest-rate) (* risk-premium u10)))
            (monthly-payment (calculate-monthly-payment loan-amount interest-rate term-length))
        )
        (asserts! (is-eq tx-sender (get owner borrower)) err-unauthorized)
        (asserts! (<= loan-amount (get current-value equipment)) err-insufficient-collateral)
        (asserts! (> loan-amount u0) err-invalid-amount)
        (asserts! (get active equipment) err-equipment-inactive)
        
        (map-set equipment-loans loan-id {
            borrower-id: borrower-id,
            equipment-id: equipment-id,
            loan-amount: loan-amount,
            interest-rate: interest-rate,
            term-length: term-length,
            monthly-payment: monthly-payment,
            remaining-balance: loan-amount,
            payments-made: u0,
            last-payment-date: u0,
            next-payment-due: (+ block-height u4320), ;; ~30 days
            loan-status: "active",
            originated-at: block-height
        })
        
        ;; Update borrower stats
        (map-set business-borrowers borrower-id
            (merge borrower {
                total-borrowed: (+ (get total-borrowed borrower) loan-amount),
                active-loans: (+ (get active-loans borrower) u1)
            })
        )
        
        (var-set total-loans-originated (+ (var-get total-loans-originated) u1))
        (var-set total-equipment-financed (+ (var-get total-equipment-financed) loan-amount))
        (var-set next-loan-id (+ loan-id u1))
        (ok loan-id)
    )
)

;; Make Loan Payment
(define-public (make-loan-payment
    (loan-id uint)
    (payment-id uint)
    (payment-amount uint))
    (let
        (
            (loan (unwrap! (map-get? equipment-loans loan-id) err-loan-not-found))
            (borrower (unwrap! (map-get? business-borrowers (get borrower-id loan)) err-invalid-borrower))
        )
        (asserts! (is-eq tx-sender (get owner borrower)) err-unauthorized)
        (asserts! (>= payment-amount (get monthly-payment loan)) err-invalid-amount)
        
        (let
            (
                (interest-portion (/ (* (get remaining-balance loan) (get interest-rate loan)) u1200))
                (principal-portion (- payment-amount interest-portion))
                (new-balance (- (get remaining-balance loan) principal-portion))
            )
            ;; Record payment
            (map-set loan-payments { loan-id: loan-id, payment-id: payment-id } {
                amount: payment-amount,
                payment-date: block-height,
                principal: principal-portion,
                interest: interest-portion,
                late-fee: u0
            })
            
            ;; Update loan
            (map-set equipment-loans loan-id
                (merge loan {
                    remaining-balance: new-balance,
                    payments-made: (+ (get payments-made loan) u1),
                    last-payment-date: block-height,
                    next-payment-due: (+ block-height u4320),
                    loan-status: (if (is-eq new-balance u0) "paid-off" "active")
                })
            )
            
            (ok new-balance)
        )
    )
)

;; Update IoT Equipment Data
(define-public (update-iot-data
    (equipment-id uint)
    (operational-hours uint)
    (revenue-generated uint)
    (efficiency-rating uint)
    (geolocation (string-ascii 100)))
    (match (map-get? iot-sensor-data equipment-id)
        sensor-data
        (begin
            (map-set iot-sensor-data equipment-id
                (merge sensor-data {
                    operational-hours: operational-hours,
                    revenue-generated: revenue-generated,
                    efficiency-rating: efficiency-rating,
                    geolocation: geolocation,
                    last-activity: block-height
                })
            )
            
            ;; Update equipment value based on usage
            (let
                (
                    (equipment (unwrap! (map-get? commercial-equipment equipment-id) err-equipment-not-found))
                    (depreciation-rate (/ operational-hours u10000))
                    (new-value (- (get current-value equipment) depreciation-rate))
                )
                (map-set commercial-equipment equipment-id
                    (merge equipment {
                        current-value: new-value,
                        usage-hours: operational-hours,
                        last-monitored: block-height
                    })
                )
            )
            
            (ok true)
        )
        err-equipment-not-found
    )
)

;; Initiate Equipment Repossession
(define-public (initiate-repossession
    (loan-id uint)
    (reason (string-ascii 200)))
    (let
        (
            (loan (unwrap! (map-get? equipment-loans loan-id) err-loan-not-found))
            (equipment (unwrap! (map-get? commercial-equipment (get equipment-id loan)) err-equipment-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (check-payment-overdue loan-id) err-payment-overdue)
        
        (map-set repossession-orders loan-id {
            loan-id: loan-id,
            equipment-id: (get equipment-id loan),
            borrower-id: (get borrower-id loan),
            reason: reason,
            initiated-at: block-height,
            status: "pending",
            recovery-value: (get current-value equipment)
        })
        
        ;; Update loan status
        (map-set equipment-loans loan-id
            (merge loan { loan-status: "repossessed" })
        )
        
        ;; Deactivate equipment
        (map-set commercial-equipment (get equipment-id loan)
            (merge equipment { active: false })
        )
        
        (ok loan-id)
    )
)

;; Automated Payment Processing
(define-public (process-automated-payment (loan-id uint))
    (let
        (
            (loan (unwrap! (map-get? equipment-loans loan-id) err-loan-not-found))
            (iot-data (unwrap! (map-get? iot-sensor-data (get equipment-id loan)) err-equipment-not-found))
        )
        (asserts! (> (get revenue-generated iot-data) (get monthly-payment loan)) err-insufficient-collateral)
        
        (let
            (
                (payment-amount (get monthly-payment loan))
                (interest-portion (/ (* (get remaining-balance loan) (get interest-rate loan)) u1200))
                (principal-portion (- payment-amount interest-portion))
                (new-balance (- (get remaining-balance loan) principal-portion))
            )
            ;; Update loan with automated payment
            (map-set equipment-loans loan-id
                (merge loan {
                    remaining-balance: new-balance,
                    payments-made: (+ (get payments-made loan) u1),
                    last-payment-date: block-height,
                    next-payment-due: (+ block-height u4320),
                    loan-status: (if (is-eq new-balance u0) "paid-off" "active")
                })
            )
            
            ;; Deduct from equipment revenue
            (map-set iot-sensor-data (get equipment-id loan)
                (merge iot-data {
                    revenue-generated: (- (get revenue-generated iot-data) payment-amount)
                })
            )
            
            (ok new-balance)
        )
    )
)

;; Read-only Functions
(define-read-only (get-borrower (borrower-id uint))
    (map-get? business-borrowers borrower-id)
)

(define-read-only (get-equipment (equipment-id uint))
    (map-get? commercial-equipment equipment-id)
)

(define-read-only (get-loan (loan-id uint))
    (map-get? equipment-loans loan-id)
)

(define-read-only (get-iot-data (equipment-id uint))
    (map-get? iot-sensor-data equipment-id)
)

(define-read-only (get-payment (loan-id uint) (payment-id uint))
    (map-get? loan-payments { loan-id: loan-id, payment-id: payment-id })
)

(define-read-only (get-repossession-order (loan-id uint))
    (map-get? repossession-orders loan-id)
)

(define-read-only (get-platform-stats)
    {
        total-loans-originated: (var-get total-loans-originated),
        total-equipment-financed: (var-get total-equipment-financed),
        default-interest-rate: (var-get default-interest-rate)
    }
)


;; title: equipment-lender
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

