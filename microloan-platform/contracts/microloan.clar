;; microloan.clar - Decentralized Microloan Platform

;; Error codes
(define-constant ERR_UNAUTHORIZED u1)
(define-constant ERR_ALREADY_REGISTERED u2)
(define-constant ERR_LOAN_EXISTS u3)
(define-constant ERR_LOAN_NOT_FOUND u4)
(define-constant ERR_INSUFFICIENT_FUNDS u5)
(define-constant ERR_LOAN_ALREADY_FUNDED u6)
(define-constant ERR_LOAN_NOT_FUNDED u7)
(define-constant ERR_LOAN_DEFAULTED u8)
(define-constant ERR_LOAN_REPAID u9)

;; Data structures
(define-map businesses
  { owner: principal }
  { 
    name: (string-ascii 100),
    description: (string-ascii 500),
    reputation: uint
  }
)

(define-map loans
  { id: uint }
  {
    business: principal,
    amount: uint,
    term: uint,      ;; in days
    interest-rate: uint,  ;; basis points (1/100 of a percent)
    status: (string-ascii 20),  ;; "PENDING", "FUNDED", "REPAYING", "REPAID", "DEFAULTED"
    created-at: uint,
    funded-at: (optional uint),
    repaid-at: (optional uint)
  }
)

(define-map loan-funders
  { loan-id: uint, funder: principal }
  { amount: uint }
)

(define-map repayments
  { loan-id: uint, payment-id: uint }
  {
    amount: uint,
    timestamp: uint
  }
)

;; Variables
(define-data-var loan-counter uint u0)

;; Functions

;; Register a business
(define-public (register-business (name (string-ascii 100)) (description (string-ascii 500)))
  (let ((owner tx-sender))
    (if (is-some (map-get? businesses { owner: owner }))
        (err ERR_ALREADY_REGISTERED)
        (ok (map-set businesses 
                    { owner: owner } 
                    { name: name, 
                      description: description, 
                      reputation: u100 }))))) ;; Start with neutral reputation

;; Create a loan request
(define-public (create-loan (amount uint) (term uint) (interest-rate uint))
  (let ((loan-id (var-get loan-counter))
        (owner tx-sender))
    (if (is-some (map-get? businesses { owner: owner }))
        (begin
          (var-set loan-counter (+ loan-id u1))
          (map-set loans
                  { id: loan-id }
                  { business: owner,
                    amount: amount,
                    term: term,
                    interest-rate: interest-rate,
                    status: "PENDING",
                    created-at: block-height,
                    funded-at: none,
                    repaid-at: none })
          (ok loan-id))
        (err ERR_UNAUTHORIZED))))

;; Fund a loan
(define-public (fund-loan (loan-id uint) (amount uint))
  (let ((loan (map-get? loans { id: loan-id }))
        (funder tx-sender))
    (match loan
      loan-data
        (if (not (is-eq (get status loan-data) "PENDING"))
            (err ERR_LOAN_ALREADY_FUNDED)
            (begin
              ;; Transfer funds from funder to contract
              (unwrap! (stx-transfer? amount funder contract-caller) (err ERR_INSUFFICIENT_FUNDS))
              
              ;; Update loan status
              (map-set loans 
                      { id: loan-id }
                      (merge loan-data { 
                        status: "FUNDED",
                        funded-at: (some block-height) 
                      }))
              
              ;; Record funder contribution
              (map-set loan-funders
                      { loan-id: loan-id, funder: funder }
                      { amount: amount })
              
              ;; Transfer funds to business
              (unwrap! (as-contract 
                        (stx-transfer? amount contract-caller (get business loan-data)))
                      (err ERR_INSUFFICIENT_FUNDS))
              
              (ok true)))
      (err ERR_LOAN_NOT_FOUND))))

;; Make a repayment
(define-public (make-repayment (loan-id uint) (payment-id uint) (amount uint))
  (let ((loan (map-get? loans { id: loan-id }))
        (borrower tx-sender))
    (match loan
      loan-data
        (if (not (is-eq (get business loan-data) borrower))
            (err ERR_UNAUTHORIZED)
            (if (not (is-eq (get status loan-data) "FUNDED"))
                (err ERR_LOAN_NOT_FUNDED)
                (begin
                  ;; Transfer repayment to contract
                  (unwrap! (stx-transfer? amount borrower contract-caller) (err ERR_INSUFFICIENT_FUNDS))
                  
                  ;; Record repayment
                  (map-set repayments
                          { loan-id: loan-id, payment-id: payment-id }
                          { amount: amount, timestamp: block-height })
                  
                  ;; Update loan if fully repaid (this is simplified, in a real implementation 
                  ;; you would track total repaid and check against loan amount + interest)
                  (if (is-eq payment-id u1)
                      (map-set loans
                              { id: loan-id }
                              (merge loan-data {
                                status: "REPAID",
                                repaid-at: (some block-height)
                              }))
                      (map-set loans
                              { id: loan-id }
                              (merge loan-data {
                                status: "REPAYING"
                              })))
                  
                  (ok true))))
      (err ERR_LOAN_NOT_FOUND))))

;; Get business details
(define-read-only (get-business-details (owner principal))
  (map-get? businesses { owner: owner }))

;; Get loan details
(define-read-only (get-loan-details (loan-id uint))
  (map-get? loans { id: loan-id }))

;; Get loan funders
(define-read-only (get-loan-funding (loan-id uint) (funder principal))
  (map-get? loan-funders { loan-id: loan-id, funder: funder }))

;; Get repayment details
(define-read-only (get-repayment (loan-id uint) (payment-id uint))
  (map-get? repayments { loan-id: loan-id, payment-id: payment-id }))
