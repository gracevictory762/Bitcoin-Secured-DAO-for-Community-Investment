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
(define-constant err-invalid-delegation (err u111))
(define-constant err-self-delegation (err u112))
(define-constant err-no-delegation (err u113))

(define-map delegations
    principal
    principal
)

(define-map delegation-power
    principal
    uint
)

(define-public (delegate-vote (delegate-to principal))
    (begin
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (is-member delegate-to) err-not-member)
        (asserts! (not (is-eq tx-sender delegate-to)) err-self-delegation)
        (map-set delegations tx-sender delegate-to)
        (map-set delegation-power delegate-to
            (+ (default-to u0 (map-get? delegation-power delegate-to)) u1)
        )
        (ok true)
    )
)

(define-public (revoke-delegation)
    (let ((current-delegate (unwrap! (map-get? delegations tx-sender) err-no-delegation)))
        (map-delete delegations tx-sender)
        (map-set delegation-power current-delegate
            (- (default-to u1 (map-get? delegation-power current-delegate)) u1)
        )
        (ok true)
    )
)

(define-public (vote-with-delegation
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (vote-key {
                proposal-id: proposal-id,
                voter: tx-sender,
            })
            (voting-power (+ u1 (default-to u0 (map-get? delegation-power tx-sender))))
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
                (merge proposal { yes-votes: (+ (get yes-votes proposal) voting-power) })
            )
            (map-set proposals proposal-id
                (merge proposal { no-votes: (+ (get no-votes proposal) voting-power) })
            )
        )
        (ok true)
    )
)

(define-read-only (get-delegation (delegator principal))
    (map-get? delegations delegator)
)

(define-read-only (get-voting-power (member principal))
    (+ u1 (default-to u0 (map-get? delegation-power member)))
)
(define-constant err-invalid-category (err u114))
(define-constant err-category-budget-exceeded (err u115))
(define-constant err-category-not-found (err u116))

(define-data-var category-count uint u0)

(define-map categories
    uint
    {
        name: (string-ascii 50),
        budget-limit: uint,
        spent-amount: uint,
        active: bool,
    }
)

(define-map proposal-categories
    uint
    uint
)

(define-public (create-category
        (name (string-ascii 50))
        (budget-limit uint)
    )
    (let ((category-id (+ (var-get category-count) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> budget-limit u0) err-invalid-amount)
        (map-set categories category-id {
            name: name,
            budget-limit: budget-limit,
            spent-amount: u0,
            active: true,
        })
        (var-set category-count category-id)
        (ok category-id)
    )
)

(define-public (create-categorized-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (amount uint)
        (recipient principal)
        (category-id uint)
    )
    (let (
            (proposal-id (+ (var-get proposal-count) u1))
            (category (unwrap! (map-get? categories category-id) err-category-not-found))
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (<= amount (var-get dao-treasury)) err-insufficient-funds)
        (asserts! (get active category) err-invalid-category)
        (asserts!
            (<= (+ (get spent-amount category) amount)
                (get budget-limit category)
            )
            err-category-budget-exceeded
        )
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
        (map-set proposal-categories proposal-id category-id)
        (var-set proposal-count proposal-id)
        (ok proposal-id)
    )
)

(define-public (execute-categorized-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (category-id (unwrap! (map-get? proposal-categories proposal-id)
                err-category-not-found
            ))
            (category (unwrap! (map-get? categories category-id) err-category-not-found))
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
                (map-set categories category-id
                    (merge category { spent-amount: (+ (get spent-amount category) (get amount proposal)) })
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

(define-public (update-category-budget
        (category-id uint)
        (new-budget uint)
    )
    (let ((category (unwrap! (map-get? categories category-id) err-category-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-budget u0) err-invalid-amount)
        (map-set categories category-id
            (merge category { budget-limit: new-budget })
        )
        (ok true)
    )
)

(define-public (toggle-category-status (category-id uint))
    (let ((category (unwrap! (map-get? categories category-id) err-category-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set categories category-id
            (merge category { active: (not (get active category)) })
        )
        (ok true)
    )
)

(define-read-only (get-category (category-id uint))
    (map-get? categories category-id)
)

(define-read-only (get-proposal-category (proposal-id uint))
    (map-get? proposal-categories proposal-id)
)

(define-read-only (get-category-remaining-budget (category-id uint))
    (match (map-get? categories category-id)
        category (ok (- (get budget-limit category) (get spent-amount category)))
        err-category-not-found
    )
)
