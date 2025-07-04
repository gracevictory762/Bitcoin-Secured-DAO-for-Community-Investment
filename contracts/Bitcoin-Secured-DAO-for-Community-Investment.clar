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

(define-constant err-not-multisig-member (err u117))
(define-constant err-already-signed (err u118))
(define-constant err-insufficient-signatures (err u119))
(define-constant err-multisig-threshold-exceeded (err u120))

(define-data-var multisig-threshold uint u100000000)
(define-data-var required-signatures uint u3)
(define-data-var multisig-member-count uint u0)
(define-constant err-insufficient-reputation (err u121))
(define-constant err-reputation-calculation-failed (err u122))
(define-constant err-calculation-failed (err u123))

(define-data-var base-reputation uint u100)
(define-data-var contribution-multiplier uint u10)
(define-data-var proposal-success-bonus uint u50)
(define-data-var min-reputation-for-proposals uint u200)

(define-map member-reputation
    principal
    uint
)

(define-map proposal-creator-reputation
    uint
    principal
)

(define-map successful-proposals-count
    principal
    uint
)

(define-public (calculate-member-reputation (member principal))
    (let (
            (base-rep (var-get base-reputation))
            (contribution-amount (default-to u0 (map-get? member-contributions member)))
            (contribution-rep (* (/ contribution-amount u1000000) (var-get contribution-multiplier)))
            (successful-count (default-to u0 (map-get? successful-proposals-count member)))
            (success-rep (* successful-count (var-get proposal-success-bonus)))
            (total-reputation (+ base-rep (+ contribution-rep success-rep)))
        )
        (map-set member-reputation member total-reputation)
        (ok total-reputation)
    )
)

(define-public (create-reputation-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (amount uint)
        (recipient principal)
    )
    (let (
            (proposal-id (+ (var-get proposal-count) u1))
            (member-rep (default-to u0 (map-get? member-reputation tx-sender)))
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (<= amount (var-get dao-treasury)) err-insufficient-funds)
        (asserts! (>= member-rep (var-get min-reputation-for-proposals))
            err-insufficient-reputation
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
        (map-set proposal-creator-reputation proposal-id tx-sender)
        (var-set proposal-count proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-with-reputation
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (vote-key {
                proposal-id: proposal-id,
                voter: tx-sender,
            })
            (member-rep (default-to u0 (map-get? member-reputation tx-sender)))
            (voting-weight (if (< member-rep u50)
                u1
                (/ member-rep u50)
            ))
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (is-eq (get status proposal) "active") err-proposal-not-active)
        (asserts! (<= burn-block-height (get expires-at proposal))
            err-proposal-not-active
        )
        (asserts! (is-none (map-get? votes vote-key)) err-already-voted)
        (unwrap! (calculate-member-reputation tx-sender) err-calculation-failed)
        (map-set votes vote-key vote)
        (if vote
            (map-set proposals proposal-id
                (merge proposal { yes-votes: (+ (get yes-votes proposal) voting-weight) })
            )
            (map-set proposals proposal-id
                (merge proposal { no-votes: (+ (get no-votes proposal) voting-weight) })
            )
        )
        (ok true)
    )
)

(define-public (execute-reputation-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (creator (unwrap! (map-get? proposal-creator-reputation proposal-id)
                err-proposal-not-found
            ))
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
            (yes-percentage (if (> total-votes u0)
                (/ (* (get yes-votes proposal) u100) total-votes)
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
                (map-set successful-proposals-count creator
                    (+
                        (default-to u0
                            (map-get? successful-proposals-count creator)
                        )
                        u1
                    ))
                (unwrap-panic (calculate-member-reputation creator))
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

(define-public (update-reputation-parameters
        (new-base uint)
        (new-multiplier uint)
        (new-bonus uint)
        (new-min-reputation uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set base-reputation new-base)
        (var-set contribution-multiplier new-multiplier)
        (var-set proposal-success-bonus new-bonus)
        (var-set min-reputation-for-proposals new-min-reputation)
        (ok true)
    )
)

(define-public (refresh-all-member-reputation)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok true)
    )
)

(define-read-only (get-member-reputation (member principal))
    (default-to u0 (map-get? member-reputation member))
)

(define-read-only (get-member-voting-weight (member principal))
    (let ((reputation (default-to u0 (map-get? member-reputation member))))
        (if (< (/ reputation u50) u1)
            u1
            (/ reputation u50)
        )
    )
)

(define-read-only (get-successful-proposals-count (member principal))
    (default-to u0 (map-get? successful-proposals-count member))
)

(define-read-only (can-create-proposal (member principal))
    (let ((reputation (default-to u0 (map-get? member-reputation member))))
        (>= reputation (var-get min-reputation-for-proposals))
    )
)

(define-read-only (get-reputation-requirements)
    {
        base-reputation: (var-get base-reputation),
        contribution-multiplier: (var-get contribution-multiplier),
        proposal-success-bonus: (var-get proposal-success-bonus),
        min-reputation-for-proposals: (var-get min-reputation-for-proposals),
    }
)
(define-map multisig-members
    principal
    bool
)

(define-map proposal-signatures
    {
        proposal-id: uint,
        signer: principal,
    }
    bool
)

(define-map proposal-signature-count
    uint
    uint
)

(define-public (add-multisig-member (member principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-member member) err-not-member)
        (asserts! (not (default-to false (map-get? multisig-members member)))
            err-already-member
        )
        (map-set multisig-members member true)
        (var-set multisig-member-count (+ (var-get multisig-member-count) u1))
        (ok true)
    )
)

(define-public (remove-multisig-member (member principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (default-to false (map-get? multisig-members member))
            err-not-multisig-member
        )
        (map-delete multisig-members member)
        (var-set multisig-member-count (- (var-get multisig-member-count) u1))
        (ok true)
    )
)

(define-public (sign-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (signature-key {
                proposal-id: proposal-id,
                signer: tx-sender,
            })
        )
        (asserts! (default-to false (map-get? multisig-members tx-sender))
            err-not-multisig-member
        )
        (asserts! (is-eq (get status proposal) "approved")
            err-proposal-not-active
        )
        (asserts! (is-none (map-get? proposal-signatures signature-key))
            err-already-signed
        )
        (map-set proposal-signatures signature-key true)
        (map-set proposal-signature-count proposal-id
            (+ (default-to u0 (map-get? proposal-signature-count proposal-id)) u1)
        )
        (ok true)
    )
)

(define-public (execute-multisig-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (signature-count (default-to u0 (map-get? proposal-signature-count proposal-id)))
            (requires-multisig (>= (get amount proposal) (var-get multisig-threshold)))
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (is-eq (get status proposal) "approved")
            err-proposal-not-active
        )
        (if requires-multisig
            (asserts! (>= signature-count (var-get required-signatures))
                err-insufficient-signatures
            )
            true
        )
        (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
        (var-set dao-treasury (- (var-get dao-treasury) (get amount proposal)))
        (map-set proposals proposal-id (merge proposal { status: "executed" }))
        (ok true)
    )
)

(define-public (approve-proposal-for-multisig (proposal-id uint))
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
                (map-set proposals proposal-id
                    (merge proposal { status: "approved" })
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

(define-public (update-multisig-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set multisig-threshold new-threshold)
        (ok true)
    )
)

(define-public (update-required-signatures (new-count uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-count (var-get multisig-member-count))
            err-multisig-threshold-exceeded
        )
        (var-set required-signatures new-count)
        (ok true)
    )
)

(define-read-only (is-multisig-member (member principal))
    (default-to false (map-get? multisig-members member))
)

(define-read-only (get-proposal-signature-count (proposal-id uint))
    (default-to u0 (map-get? proposal-signature-count proposal-id))
)

(define-read-only (has-signed-proposal
        (proposal-id uint)
        (signer principal)
    )
    (default-to false
        (map-get? proposal-signatures {
            proposal-id: proposal-id,
            signer: signer,
        })
    )
)

(define-read-only (requires-multisig (amount uint))
    (>= amount (var-get multisig-threshold))
)
