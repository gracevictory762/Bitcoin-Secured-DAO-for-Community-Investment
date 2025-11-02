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
(define-constant err-invalid-timeframe (err u111))
(define-constant err-quorum-not-met (err u112))

(define-data-var dao-treasury uint u0)
(define-data-var membership-fee uint u1000000)
(define-data-var proposal-count uint u0)
(define-data-var voting-period uint u144)
(define-data-var execution-threshold uint u51)
(define-data-var quorum-percentage uint u20)

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

;; Voting History Tracking - New Feature
(define-map member-voting-stats
    principal
    {
        total-votes-cast: uint,
        yes-votes: uint,
        no-votes: uint,
        proposals-created: uint,
        last-vote-block: uint,
        participation-score: uint,
    }
)

(define-map proposal-participation
    uint
    {
        total-eligible-voters: uint,
        actual-voters: uint,
        participation-rate: uint,
        avg-member-contribution: uint,
    }
)

(define-data-var total-dao-votes uint u0)
(define-data-var total-active-members uint u0)

;; Public functions

(define-public (join-dao)
    (let ((current-fee (var-get membership-fee)))
        (asserts! (not (default-to false (map-get? members tx-sender)))
            err-already-member
        )
        (try! (stx-transfer? current-fee tx-sender (as-contract tx-sender)))
        (map-set members tx-sender true)
        (map-set member-contributions tx-sender u0)
        ;; Initialize voting statistics for new member
        (map-set member-voting-stats tx-sender {
            total-votes-cast: u0,
            yes-votes: u0,
            no-votes: u0,
            proposals-created: u0,
            last-vote-block: u0,
            participation-score: u0,
        })
        (var-set dao-treasury (+ (var-get dao-treasury) current-fee))
        (var-set total-active-members (+ (var-get total-active-members) u1))
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
    (let (
            (proposal-id (+ (var-get proposal-count) u1))
            (current-stats (default-to {
                total-votes-cast: u0,
                yes-votes: u0,
                no-votes: u0,
                proposals-created: u0,
                last-vote-block: u0,
                participation-score: u0,
            }
                (map-get? member-voting-stats tx-sender)
            ))
        )
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
        ;; Update creator's proposal creation stats
        (map-set member-voting-stats tx-sender
            (merge current-stats { proposals-created: (+ (get proposals-created current-stats) u1) })
        )
        ;; Initialize proposal participation tracking
        (map-set proposal-participation proposal-id {
            total-eligible-voters: (var-get total-active-members),
            actual-voters: u0,
            participation-rate: u0,
            avg-member-contribution: u0,
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
            (current-stats (default-to {
                total-votes-cast: u0,
                yes-votes: u0,
                no-votes: u0,
                proposals-created: u0,
                last-vote-block: u0,
                participation-score: u0,
            }
                (map-get? member-voting-stats tx-sender)
            ))
            (current-participation (unwrap! (map-get? proposal-participation proposal-id)
                err-proposal-not-found
            ))
        )
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (is-eq (get status proposal) "active") err-proposal-not-active)
        (asserts! (<= burn-block-height (get expires-at proposal))
            err-proposal-not-active
        )
        (asserts! (is-none (map-get? votes vote-key)) err-already-voted)
        (map-set votes vote-key vote)

        ;; Update proposal vote counts and member stats
        (if vote
            (begin
                (map-set proposals proposal-id
                    (merge proposal { yes-votes: (+ (get yes-votes proposal) u1) })
                )
                (map-set member-voting-stats tx-sender
                    (merge current-stats {
                        total-votes-cast: (+ (get total-votes-cast current-stats) u1),
                        yes-votes: (+ (get yes-votes current-stats) u1),
                        last-vote-block: burn-block-height,
                        participation-score: (+ (get participation-score current-stats) u2), ;; Bonus for voting yes
                    })
                )
            )
            (begin
                (map-set proposals proposal-id
                    (merge proposal { no-votes: (+ (get no-votes proposal) u1) })
                )
                (map-set member-voting-stats tx-sender
                    (merge current-stats {
                        total-votes-cast: (+ (get total-votes-cast current-stats) u1),
                        no-votes: (+ (get no-votes current-stats) u1),
                        last-vote-block: burn-block-height,
                        participation-score: (+ (get participation-score current-stats) u1), ;; Base score for voting
                    })
                )
            )
        )

        ;; Update proposal participation statistics
        (let ((new-voter-count (+ (get actual-voters current-participation) u1)))
            (map-set proposal-participation proposal-id
                (merge current-participation {
                    actual-voters: new-voter-count,
                    participation-rate: (/ (* new-voter-count u100)
                        (get total-eligible-voters current-participation)
                    ),
                })
            )
        )

        (var-set total-dao-votes (+ (var-get total-dao-votes) u1))
        (ok true)
    )
)
(define-public (execute-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
            (eligible-voters (var-get total-active-members))
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
        (asserts! (meets-quorum total-votes eligible-voters) err-quorum-not-met)
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

(define-public (set-quorum-percentage (new-quorum uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-quorum u100) err-invalid-amount)
        (var-set quorum-percentage new-quorum)
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

(define-read-only (get-quorum-percentage)
    (var-get quorum-percentage)
)

(define-private (meets-quorum
        (voted uint)
        (eligible uint)
    )
    (>= (* voted u100) (* eligible (var-get quorum-percentage)))
)

;; Voting History Tracker Read-Only Functions

(define-read-only (get-member-voting-stats (member principal))
    (map-get? member-voting-stats member)
)

(define-read-only (get-proposal-participation (proposal-id uint))
    (map-get? proposal-participation proposal-id)
)

(define-read-only (get-dao-voting-summary)
    {
        total-votes-cast: (var-get total-dao-votes),
        total-active-members: (var-get total-active-members),
        total-proposals: (var-get proposal-count),
        average-participation: (if (> (var-get proposal-count) u0)
            (/ (var-get total-dao-votes) (var-get proposal-count))
            u0
        ),
    }
)

(define-read-only (get-member-participation-rate (member principal))
    (let (
            (stats (default-to {
                total-votes-cast: u0,
                yes-votes: u0,
                no-votes: u0,
                proposals-created: u0,
                last-vote-block: u0,
                participation-score: u0,
            }
                (map-get? member-voting-stats member)
            ))
            (total-proposals (var-get proposal-count))
        )
        (if (and (> total-proposals u0) (is-member member))
            (/ (* (get total-votes-cast stats) u100) total-proposals)
            u0
        )
    )
)

(define-read-only (get-top-contributors (limit uint))
    ;; This function would return a simplified view
    ;; In a full implementation, you'd use a sorted data structure
    {
        total-active-members: (var-get total-active-members),
        total-proposals-created: (var-get proposal-count),
        evaluation-note: "Use get-member-voting-stats for individual member details",
    }
)

(define-read-only (get-proposal-stats-range
        (start-id uint)
        (end-id uint)
    )
    (if (and (<= start-id end-id) (<= end-id (var-get proposal-count)))
        (ok {
            range-start: start-id,
            range-end: end-id,
            total-proposals-in-range: (+ (- end-id start-id) u1),
            note: "Use get-proposal-participation for individual proposal details",
        })
        err-invalid-timeframe
    )
)

(define-read-only (calculate-member-engagement-score (member principal))
    (let (
            (stats (default-to {
                total-votes-cast: u0,
                yes-votes: u0,
                no-votes: u0,
                proposals-created: u0,
                last-vote-block: u0,
                participation-score: u0,
            }
                (map-get? member-voting-stats member)
            ))
            (member-contribution (get-member-contribution member))
            (total-proposals (var-get proposal-count))
        )
        (if (is-member member)
            {
                voting-participation: (get participation-score stats),
                proposal-creation-bonus: (* (get proposals-created stats) u5),
                financial-contribution: (/ member-contribution u1000000), ;; STX in whole units
                total-engagement: (+ (get participation-score stats)
                    (* (get proposals-created stats) u5)
                    (/ member-contribution u1000000)
                ),
                last-activity-block: (get last-vote-block stats),
            }
            {
                voting-participation: u0,
                proposal-creation-bonus: u0,
                financial-contribution: u0,
                total-engagement: u0,
                last-activity-block: u0,
            }
        )
    )
)
