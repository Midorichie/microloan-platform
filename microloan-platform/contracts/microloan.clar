;; microloan.clar - Decentralized Microloan Platform

;; Error codes
(define-constant ERR_UNAUTHORIZED u1)
(define-constant ERR_ALREADY_REGISTERED u2)
(define-constant ERR_LOAN_NOT_FOUND u4)
(define-constant ERR_INSUFFICIENT_FUNDS u5)
(define-constant ERR_LOAN_ALREADY_FUNDED u6)
(define-constant ERR_LOAN_NOT_FUNDED u7)
(define-constant ERR_INVALID_AMOUNT u10)
(define-constant ERR_INVALID_TERM u11)
(define-constant ERR_INVALID_RATE u12)
(define-constant ERR_INVALID_PAYMENT_ID u13)
(define-constant ERR_INVALID_INPUT u14)

;; Constants
(define-constant MIN_LOAN_AMOUNT u10000)
(define-constant MAX_LOAN_AMOUNT u1000000000)
(define-constant MIN_LOAN_TERM u1)
(define-constant MAX_LOAN_TERM u365)
(define-constant MAX_INTEREST_RATE u10000)

;; Data maps
(define-map businesses 
  { owner: principal }
  { name: (string-ascii 100), description: (string-ascii 500), reputation: uint }
)

(define-map loans 
  { id: uint }
  { business: principal, amount: uint, term: uint, interest_rate: uint,
    status: (string-ascii 20), created_at: uint, funded_at: (optional uint),
    repaid_at: (optional uint), repayment_count: uint, total_repaid: uint }
)

;; Variables
(define-data-var loan_counter uint u0)

;; Helper functions
(define-private (validate-loan-amount (amount uint))
  (and (>= amount MIN_LOAN_AMOUNT) (<= amount MAX_LOAN_AMOUNT))
)

(define-private (validate-loan-term (term uint))
  (and (>= term MIN_LOAN_TERM) (<= term MAX_LOAN_TERM))
)

(define-private (validate-interest-rate (rate uint))
  (<= rate MAX_INTEREST_RATE)
)

(define-private (validate-string-not-empty (s (string-ascii 500)))
  (> (len s) u0)
)

(define-private (validate-loan-id (loan-id uint))
  (is-some (map-get? loans { id: loan-id }))
)

(define-private (calculate-total-due (principal uint) (interest_rate uint) (term uint))
  (let ((interest (/ (* (* principal interest_rate) term) (* u10000 u365))))
    (+ principal interest)
  )
)

;; Register a business
(define-public (register-business (name (string-ascii 100)) (description (string-ascii 500)))
  (let ((owner tx-sender))
    (asserts! (validate-string-not-empty name) (err ERR_INVALID_INPUT))
    (asserts! (validate-string-not-empty description) (err ERR_INVALID_INPUT))
    (if (is-some (map-get? businesses { owner: owner }))
        (err ERR_ALREADY_REGISTERED)
        (ok (map-set businesses { owner: owner }
             { name: name, description: description, reputation: u100 })))
  )
)

;; Create a loan request
(define-public (create-loan (amount uint) (term uint) (interest-rate uint))
  (let ((owner tx-sender)
        (loan-id (var-get loan_counter)))
    (asserts! (validate-loan-amount amount) (err ERR_INVALID_AMOUNT))
    (asserts! (validate-loan-term term) (err ERR_INVALID_TERM))
    (asserts! (validate-interest-rate interest-rate) (err ERR_INVALID_RATE))
    (if (is-some (map-get? businesses { owner: owner }))
        (begin
          (var-set loan_counter (+ loan-id u1))
          (map-set loans { id: loan-id }
            { business: owner, amount: amount, term: term, interest_rate: interest-rate,
              status: "PENDING", created_at: block-height, funded_at: none,
              repaid_at: none, repayment_count: u0, total_repaid: u0 })
          (ok loan-id))
        (err ERR_UNAUTHORIZED))
  )
)

;; Fund a loan
(define-public (fund-loan (loan-id uint) (amount uint))
  (let ((funder tx-sender))
    (asserts! (validate-loan-id loan-id) (err ERR_LOAN_NOT_FOUND))
    (asserts! (validate-loan-amount amount) (err ERR_INVALID_AMOUNT))
    (let ((loan (unwrap! (map-get? loans { id: loan-id }) (err ERR_LOAN_NOT_FOUND))))
      (if (not (is-eq (get status loan) "PENDING"))
          (err ERR_LOAN_ALREADY_FUNDED)
          (begin
            (asserts! (is-eq amount (get amount loan)) (err ERR_INVALID_AMOUNT))
            (unwrap! (stx-transfer? amount funder contract-caller) (err ERR_INSUFFICIENT_FUNDS))
            (map-set loans { id: loan-id }
              (merge loan { status: "FUNDED", funded_at: (some block-height) }))
            (ok true)
          )
      )
    )
  )
)

;; Make a repayment (simplified, without lender distribution)
(define-public (make-repayment (loan-id uint) (payment-id uint) (amount uint))
  (let ((borrower tx-sender))
    (asserts! (validate-loan-id loan-id) (err ERR_LOAN_NOT_FOUND))
    (asserts! (> payment-id u0) (err ERR_INVALID_PAYMENT_ID))
    (asserts! (validate-loan-amount amount) (err ERR_INVALID_AMOUNT))
    (let ((loan (unwrap! (map-get? loans { id: loan-id }) (err ERR_LOAN_NOT_FOUND))))
      (if (not (is-eq (get business loan) borrower))
          (err ERR_UNAUTHORIZED)
          (if (not (or (is-eq (get status loan) "FUNDED")
                       (is-eq (get status loan) "REPAYING")))
              (err ERR_LOAN_NOT_FUNDED)
              (begin
                (unwrap! (stx-transfer? amount borrower contract-caller) (err ERR_INSUFFICIENT_FUNDS))
                (let ((new_total (+ (get total_repaid loan) amount))
                      (total_due (calculate-total-due (get amount loan)
                                                      (get interest_rate loan)
                                                      (get term loan))))
                  (if (>= new_total total_due)
                      (map-set loans { id: loan-id }
                        (merge loan { status: "REPAID", repaid_at: (some block-height),
                                       repayment_count: (+ (get repayment_count loan) u1),
                                       total_repaid: new_total }))
                      (map-set loans { id: loan-id }
                        (merge loan { status: "REPAYING",
                                       repayment_count: (+ (get repayment_count loan) u1),
                                       total_repaid: new_total })))
                  (ok true)
                )
              )
          )
      )
    )
  )
)
