(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-already-member (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-proposal-not-found (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-proposal-not-active (err u106))
(define-constant err-proposal-execution-failed (err u107))
(define-constant err-invalid-amount (err u108))
(define-constant err-proposal-in-progress (err u109))
(define-constant err-not-proposal-creator (err u110))

(define-data-var dao-treasury uint u0)
(define-data-var membership-fee uint u1000000) ;; 1 STX
(define-data-var proposal-count uint u0)
(define-data-var voting-period uint u144) ;; ~1 day in blocks
(define-data-var execution-threshold uint u51) ;; 51% majority

(define-map members
    principal
    bool
)
(define-map member-contributions
    principal
    uint
)

(define-map proposals
    uint
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        amount: uint,
        recipient: principal,
        status: (string-ascii 20),
        yes-votes: uint,
        no-votes: uint,
        created-at: uint,
        expires-at: uint,
    }
)

(define-map votes
    {
        proposal-id: uint,
        voter: principal,
    }
    bool
)

;; Public functions

(define-public (join-dao)
    (let ((current-fee (var-get membership-fee)))
        (asserts! (not (default-to false (map-get? members tx-sender)))
            err-already-member
        )
        (try! (stx-transfer? current-fee tx-sender (as-contract tx-sender)))
        (map-set members tx-sender true)
        (map-set member-contributions tx-sender u0)
        (var-set dao-treasury (+ (var-get dao-treasury) current-fee))
        (ok true)
    )
)

(define-public (contribute-funds (amount uint))
    (begin
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set member-contributions tx-sender
            (+ (default-to u0 (map-get? member-contributions tx-sender)) amount)
        )
        (var-set dao-treasury (+ (var-get dao-treasury) amount))
        (ok true)
    )
)

(define-public (create-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (amount uint)
        (recipient principal)
    )
    (let ((proposal-id (+ (var-get proposal-count) u1)))
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (<= amount (var-get dao-treasury)) err-insufficient-funds)
        (map-set proposals proposal-id {
            creator: tx-sender,
            title: title,
            description: description,
            amount: amount,
            recipient: recipient,
            status: "active",
            yes-votes: u0,
            no-votes: u0,
            created-at: burn-block-height,
            expires-at: (+ burn-block-height (var-get voting-period)),
        })
        (var-set proposal-count proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (vote-key {
                proposal-id: proposal-id,
                voter: tx-sender,
            })
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (is-eq (get status proposal) "active") err-proposal-not-active)
        (asserts! (<= burn-block-height (get expires-at proposal))
            err-proposal-not-active
        )
        (asserts! (is-none (map-get? votes vote-key)) err-already-voted)
        (map-set votes vote-key vote)
        (if vote
            (map-set proposals proposal-id
                (merge proposal { yes-votes: (+ (get yes-votes proposal) u1) })
            )
            (map-set proposals proposal-id
                (merge proposal { no-votes: (+ (get no-votes proposal) u1) })
            )
        )
        (ok true)
    )
)
(define-public (execute-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
            (yes-percentage (if (> total-votes u0)
                (* (get yes-votes proposal) u100)
                u0
            ))
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (is-eq (get status proposal) "active") err-proposal-not-active)
        (asserts! (>= burn-block-height (get expires-at proposal))
            err-proposal-in-progress
        )
        (if (>= yes-percentage (var-get execution-threshold))
            (begin
                (try! (as-contract (stx-transfer? (get amount proposal) tx-sender
                    (get recipient proposal)
                )))
                (var-set dao-treasury
                    (- (var-get dao-treasury) (get amount proposal))
                )
                (map-set proposals proposal-id
                    (merge proposal { status: "executed" })
                )
                (ok true)
            )
            (begin
                (map-set proposals proposal-id
                    (merge proposal { status: "rejected" })
                )
                (ok false)
            )
        )
    )
)
(define-public (cancel-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found)))
        (asserts! (is-eq tx-sender (get creator proposal))
            err-not-proposal-creator
        )
        (asserts! (is-eq (get status proposal) "active") err-proposal-not-active)
        (map-set proposals proposal-id (merge proposal { status: "cancelled" }))
        (ok true)
    )
)

(define-public (update-membership-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set membership-fee new-fee)
        (ok true)
    )
)

(define-public (update-voting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set voting-period new-period)
        (ok true)
    )
)

(define-public (update-execution-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-threshold u100) err-invalid-amount)
        (var-set execution-threshold new-threshold)
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-member-status (address principal))
    (default-to false (map-get? members address))
)

(define-read-only (get-dao-treasury)
    (var-get dao-treasury)
)

(define-read-only (get-member-contribution (address principal))
    (default-to u0 (map-get? member-contributions address))
)

(define-read-only (get-vote
        (proposal-id uint)
        (voter principal)
    )
    (map-get? votes {
        proposal-id: proposal-id,
        voter: voter,
    })
)

(define-read-only (is-member (address principal))
    (default-to false (map-get? members address))
)
