
;; title: pet-service-booking
;; version:
;; summary:
;; description:

;; Pet Service Booking System

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Define the fungible token for payments
(define-fungible-token service-token)

;; Constants for fee distribution
(define-constant service-provider-fee-percent u80)
(define-constant platform-fee-percent u20)

;; Map to store service provider information
(define-map service-providers
  { provider-id: principal }
  { registered: bool, total-earnings: uint })

;; Map to store service information
(define-map services
  { service-id: uint }
  { provider: principal, service-type: (string-ascii 20), price: uint, availability: bool })

;; Map to store bookings
(define-map bookings
  { booking-id: uint }
  { user: principal, provider: principal, service-id: uint, amount: uint, status: (string-ascii 10) })

;; Counter for service IDs
(define-data-var service-id-counter uint u0)

;; Counter for booking IDs
(define-data-var booking-id-counter uint u0)

;; Function to register a pet service
(define-public (register-pet-service (service-type (string-ascii 20)) (price uint))
  (let
    (
      (provider tx-sender)
      (new-service-id (+ (var-get service-id-counter) u1))
    )
    (begin
      ;; Check if the provider is already registered
      (asserts! (is-none (map-get? service-providers { provider-id: provider })) (err u400))
      
      ;; Register the service provider
      (map-set service-providers
        { provider-id: provider }
        { registered: true, total-earnings: u0 }
      )
      
      ;; Register the service
      (map-set services
        { service-id: new-service-id }
        { provider: provider, service-type: service-type, price: price, availability: true }
      )
      
      ;; Increment service ID counter
      (var-set service-id-counter new-service-id)
      
      (ok new-service-id)
    )
  )
)

;; Function to update service availability
(define-public (update-service-availability (service-id uint) (available bool))
  (let
    (
      (provider tx-sender)
    )
    (begin
      ;; Check if the service exists and belongs to the provider
      (asserts! (is-eq provider (get provider (unwrap! (map-get? services { service-id: service-id }) (err u404)))) (err u401))
      
      ;; Update the service availability
      (map-set services
        { service-id: service-id }
        (merge (unwrap-panic (map-get? services { service-id: service-id }))
               { availability: available })
      )
      
      (ok true)
    )
  )
)

;; Function to book a pet service
(define-public (book-pet-service (service-id uint))
  (let
    (
      (user tx-sender)
      (service (unwrap! (map-get? services { service-id: service-id }) (err u404)))
      (provider (get provider service))
      (price (get price service))
      (new-booking-id (+ (var-get booking-id-counter) u1))
    )
    (begin
      ;; Check if the service is available
      (asserts! (get availability service) (err u402))
      
      ;; Transfer payment from user to contract
      (try! (ft-transfer? service-token price user (as-contract tx-sender)))
      
      ;; Create the booking
      (map-set bookings
        { booking-id: new-booking-id }
        { user: user, provider: provider, service-id: service-id, amount: price, status: "booked" }
      )
      
      ;; Increment booking ID counter
      (var-set booking-id-counter new-booking-id)
      
      ;; Update service availability
      (try! (update-service-availability service-id false))
      
      (ok new-booking-id)
    )
  )
)

;; Function to complete a service and process payment
(define-public (complete-service (booking-id uint))
  (let
    (
      (provider tx-sender)
      (booking (unwrap! (map-get? bookings { booking-id: booking-id }) (err u404)))
    )
    (begin
      ;; Check if the provider is the one who provided the service
      (asserts! (is-eq provider (get provider booking)) (err u401))
      
      ;; Check if the booking status is "booked"
      (asserts! (is-eq (get status booking) "booked") (err u403))
      
      ;; Process the payment
      (try! (split-payment provider (get amount booking)))
      
      ;; Update booking status
      (map-set bookings
        { booking-id: booking-id }
        (merge booking { status: "completed" })
      )
      
      ;; Update service availability
      (try! (update-service-availability (get service-id booking) true))
      
      (ok true)
    )
  )
)

;; Function to split payment between service provider and platform
(define-private (split-payment (provider principal) (amount uint))
  (let
    (
      (provider-fee (/ (* amount service-provider-fee-percent) u100))
      (platform-fee (/ (* amount platform-fee-percent) u100))
    )
    (begin
      ;; Transfer provider fee
      (try! (as-contract (ft-transfer? service-token provider-fee tx-sender provider)))
      
      ;; Transfer platform fee
      (try! (as-contract (ft-transfer? service-token platform-fee tx-sender contract-owner)))
      
      ;; Update provider's total earnings
      (map-set service-providers
        { provider-id: provider }
        (merge (unwrap-panic (map-get? service-providers { provider-id: provider }))
               { total-earnings: (+ (get total-earnings (unwrap-panic (map-get? service-providers { provider-id: provider }))) provider-fee) })
      )
      
      (ok true)
    )
  )
)

;; Function to get service details
(define-read-only (get-service-details (service-id uint))
  (map-get? services { service-id: service-id })
)

;; Function to get booking details
(define-read-only (get-booking-details (booking-id uint))
  (map-get? bookings { booking-id: booking-id })
)

;; Function to get service provider details
(define-read-only (get-provider-details (provider-id principal))
  (map-get? service-providers { provider-id: provider-id })
)

;; Initialize the contract
(begin
  ;; Mint initial supply of service tokens to contract owner
  (try! (ft-mint? service-token u1000000000 contract-owner))
)

