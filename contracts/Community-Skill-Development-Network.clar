;; Scholarly Research Validation Network: Decentralized academic paper review system
;; Enables researchers to submit papers, reviewers to evaluate, and institutions to certify quality

(define-data-var lead-institution principal tx-sender)

(define-map research-registry
  { paper-id: uint }
  {
    author: principal,
    review-cost: uint,
    paper-title: (string-ascii 50),
    abstract-content: (string-ascii 500),
    review-period: uint,
    certified: bool
  })

(define-map evaluation-records
  { paper-id: uint, record-id: uint }
  {
    reviewer: principal,
    submission-date: uint,
    status: (string-ascii 20)
  })

(define-data-var next-paper-id uint u1)

(define-map record-tracker
  { paper-id: uint }
  { entries: uint })

;; Submit a research paper for review
(define-public (submit-paper (title-input (string-ascii 50)) (abstract-input (string-ascii 500)) (period-input uint) (cost-input uint))
  (let
    (
      (paper-id (var-get next-paper-id))
      (record-id u0)
      (title title-input)
      (abstract abstract-input)
      (period period-input)
      (cost cost-input)
    )
    ;; Input validation
    (asserts! (> cost u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len abstract) u0) (err u6))
    (asserts! (> period u0) (err u7))
    
    (map-set research-registry
      { paper-id: paper-id }
      {
        author: tx-sender,
        review-cost: cost,
        paper-title: title,
        abstract-content: abstract,
        review-period: period,
        certified: false
      })
    
    (map-set evaluation-records
      { paper-id: paper-id, record-id: record-id }
      {
        reviewer: tx-sender,
        submission-date: paper-id,
        status: "submitted"
      })
    
    (map-set record-tracker
      { paper-id: paper-id }
      { entries: u1 })
    
    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  ))

;; Review a research paper
(define-public (review-paper (paper-id-input uint))
  (let
    (
      (paper-id paper-id-input)
      (paper-info (unwrap! (map-get? research-registry { paper-id: paper-id }) (err u2)))
      (cost (get review-cost paper-info))
      (author (get author paper-info))
      (record-data (default-to { entries: u0 } (map-get? record-tracker { paper-id: paper-id })))
      (record-id (get entries record-data))
      (new-record-id (+ record-id u1))
    )
    ;; Input validation
    (asserts! (> paper-id u0) (err u8))
    (asserts! (not (is-eq tx-sender author)) (err u3))
    
    (try! (stx-transfer? cost tx-sender author))
    
    (map-set evaluation-records
      { paper-id: paper-id, record-id: record-id }
      {
        reviewer: tx-sender,
        submission-date: (var-get next-paper-id),
        status: "reviewed"
      })
    
    (map-set record-tracker
      { paper-id: paper-id }
      { entries: new-record-id })
    
    (ok true)
  ))

;; Certify a paper (lead institution only)
(define-public (certify-paper (paper-id-input uint))
  (let
    (
      (paper-id paper-id-input)
      (paper-info (unwrap! (map-get? research-registry { paper-id: paper-id }) (err u2)))
      (record-data (default-to { entries: u0 } (map-get? record-tracker { paper-id: paper-id })))
      (record-id (get entries record-data))
      (new-record-id (+ record-id u1))
    )
    ;; Input validation
    (asserts! (> paper-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get lead-institution)) (err u4))
    
    (map-set research-registry
      { paper-id: paper-id }
      (merge paper-info { certified: true }))
    
    (map-set evaluation-records
      { paper-id: paper-id, record-id: record-id }
      {
        reviewer: (get author paper-info),
        submission-date: (var-get next-paper-id),
        status: "certified"
      })
    
    (map-set record-tracker
      { paper-id: paper-id }
      { entries: new-record-id })
    
    (ok true)
  ))

;; Get paper details
(define-read-only (get-paper (paper-id uint))
  (map-get? research-registry { paper-id: paper-id }))

;; Get evaluation record
(define-read-only (get-evaluation-record (paper-id uint) (record-id uint))
  (map-get? evaluation-records { paper-id: paper-id, record-id: record-id }))

;; Get total evaluation records for a paper
(define-read-only (get-evaluation-count (paper-id uint))
  (let
    (
      (record-data (default-to { entries: u0 } (map-get? record-tracker { paper-id: paper-id })))
    )
    (get entries record-data)
  ))
