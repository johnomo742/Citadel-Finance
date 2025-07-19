;; Citadel-Finance-Lending-Protocol

(define-constant protocol-admin tx-sender)
(define-constant err-admin-only (err u100))
(define-constant err-invalid-oracle-feed (err u101))
(define-constant err-insufficient-stellar-backing (err u102))
(define-constant err-invalid-vault-state (err u103))
(define-constant err-liquidation-threshold-breached (err u104))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          DATA VARS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-data-var minimum-collateral-ratio uint u150)
(define-data-var stellar-token-price uint u0)
(define-data-var last-oracle-update uint u0)
(define-data-var nexus-protocol-active bool true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          DATA STORAGE                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-map stellar-vault-deposits
    {vault-owner: principal}      
    {stellar-tokens: uint})           

(define-map nexus-debt-ledger
    {borrower: principal}      
    {debt-amount: uint})    

(define-map protocol-yield-vault
    {yield-recipient: principal}
    {accumulated-yield: uint})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ORACLE FUNCTIONS                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-stellar-price)
    (ok (var-get stellar-token-price)))

(define-read-only (get-oracle-timestamp)
    (ok (var-get last-oracle-update)))

(define-public (update-stellar-oracle (new-price uint))
    (begin
        (asserts! (is-eq tx-sender protocol-admin) err-admin-only)
        (asserts! (> new-price u0) err-invalid-oracle-feed)
        (var-set stellar-token-price new-price)
        (var-set last-oracle-update block-height)
        (ok new-price)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          READ-ONLY FUNCTIONS                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-vault-deposits (vault-owner principal))
    (default-to u0 (get stellar-tokens (map-get? stellar-vault-deposits {vault-owner: vault-owner}))))

(define-read-only (get-debt-position (borrower principal))
    (default-to u0 (get debt-amount (map-get? nexus-debt-ledger {borrower: borrower}))))

(define-read-only (calculate-stellar-value (token-amount uint))
    (* token-amount (unwrap-panic (get-stellar-price))))

(define-read-only (verify-collateral-backing (vault-owner principal) (requested-debt uint))
    (let ((stellar-deposits (get-vault-deposits vault-owner))
          (current-price (unwrap-panic (get-stellar-price)))
          (required-ratio (var-get minimum-collateral-ratio)))
        (>= (* stellar-deposits current-price) (* requested-debt (/ required-ratio u100)))))

(define-read-only (calculate-health-factor (vault-owner principal))
    (let ((stellar-deposits (get-vault-deposits vault-owner))
          (current-debt (get-debt-position vault-owner))
          (current-price (unwrap-panic (get-stellar-price))))
        (if (is-eq current-debt u0)
            (err "No active debt position")
            (ok (/ (* stellar-deposits current-price) current-debt)))))

(define-read-only (is-protocol-active)
    (var-get nexus-protocol-active))

(define-read-only (get-collateral-ratio)
    (var-get minimum-collateral-ratio))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          PUBLIC FUNCTIONS                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (deposit-stellar-collateral (token-amount uint))
    (begin
        (asserts! (is-protocol-active) (err "Nexus protocol is inactive"))
        (asserts! (> token-amount u0) (err "Stellar deposit must be positive"))
        (map-insert stellar-vault-deposits 
                    {vault-owner: tx-sender} 
                    {stellar-tokens: (+ token-amount (default-to u0 (get stellar-tokens (map-get? stellar-vault-deposits {vault-owner: tx-sender}))))})
        (ok token-amount)))

(define-public (initiate-nexus-loan (debt-amount uint))
    (begin
        (asserts! (is-protocol-active) (err "Nexus protocol is inactive"))
        (asserts! (> debt-amount u0) (err "Debt amount must be positive"))
        (asserts! (verify-collateral-backing tx-sender debt-amount) (err "Insufficient stellar collateral backing"))
        (map-set nexus-debt-ledger
                 {borrower: tx-sender}
                 {debt-amount: (+ debt-amount (default-to u0 (get debt-amount (map-get? nexus-debt-ledger {borrower: tx-sender}))))})
        (ok debt-amount)))

(define-public (repay-nexus-debt (payment-amount uint))
    (begin
        (asserts! (is-protocol-active) (err "Nexus protocol is inactive"))
        (asserts! (> payment-amount u0) (err "Payment must be positive"))
        (let ((current-debt (get-debt-position tx-sender)))
            (asserts! (>= current-debt payment-amount) (err "Payment exceeds outstanding debt"))
            (map-set nexus-debt-ledger {borrower: tx-sender} {debt-amount: (- current-debt payment-amount)})
            (let ((stellar-price (unwrap-panic (get-stellar-price))))
                (let ((collateral-release (/ payment-amount stellar-price)))
                    (map-set stellar-vault-deposits
                             {vault-owner: tx-sender}
                             {stellar-tokens: (- (get-vault-deposits tx-sender) collateral-release)})
                    (ok collateral-release))))))

(define-public (liquidate-vault (vault-owner principal))
    (begin
        (asserts! (is-protocol-active) (err "Nexus protocol is inactive"))
        (let ((stellar-deposits (get-vault-deposits vault-owner))
              (current-debt (get-debt-position vault-owner))
              (stellar-price (unwrap-panic (get-stellar-price)))
              (required-ratio (var-get minimum-collateral-ratio)))
            (asserts! (< (* stellar-deposits stellar-price) (* current-debt (/ required-ratio u100))) 
                     (err "Position is adequately collateralized"))
            (map-delete stellar-vault-deposits {vault-owner: vault-owner})
            (map-delete nexus-debt-ledger {borrower: vault-owner})
            (ok true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ADMIN FUNCTIONS                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (configure-collateral-ratio (new-ratio uint))
    (begin
        (asserts! (is-eq tx-sender protocol-admin) (err "Access denied"))
        (asserts! (> new-ratio u100) (err "Collateral ratio must exceed 100%"))
        (var-set minimum-collateral-ratio new-ratio)
        (ok new-ratio)))

(define-public (halt-nexus-protocol)
    (begin
        (asserts! (is-eq tx-sender protocol-admin) (err "Access denied"))
        (var-set nexus-protocol-active false)
        (ok true)))

(define-public (resume-nexus-protocol)
    (begin
        (asserts! (is-eq tx-sender protocol-admin) (err "Access denied"))
        (var-set nexus-protocol-active true)
        (ok true)))

(define-public (withdraw-excess-collateral (token-amount uint))
    (begin
        (asserts! (is-protocol-active) (err "Nexus protocol is inactive"))
        (asserts! (> token-amount u0) (err "Withdrawal amount must be positive"))
        (let ((stellar-deposits (get-vault-deposits tx-sender))
              (current-debt (get-debt-position tx-sender))
              (stellar-price (unwrap-panic (get-stellar-price)))
              (required-backing (* current-debt (/ (var-get minimum-collateral-ratio) u100))))
            (asserts! (> stellar-deposits required-backing) (err "Insufficient excess collateral"))
            (let ((excess-collateral (- stellar-deposits required-backing)))
                (asserts! (>= excess-collateral token-amount) (err "Requested amount exceeds excess"))
                (map-set stellar-vault-deposits
                         {vault-owner: tx-sender}
                         {stellar-tokens: (- stellar-deposits token-amount)})
                (ok token-amount)))))

(define-public (harvest-protocol-yield)
    (begin
        (asserts! (is-protocol-active) (err "Nexus protocol is inactive"))
        (let ((yield-earned (default-to u0 (get accumulated-yield (map-get? protocol-yield-vault {yield-recipient: tx-sender})))))
            (asserts! (> yield-earned u0) (err "No yield available for harvest"))
            (map-set protocol-yield-vault {yield-recipient: tx-sender} {accumulated-yield: u0})
            (ok yield-earned))))