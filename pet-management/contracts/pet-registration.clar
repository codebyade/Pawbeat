;; title: pet-registration
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;; Error constants
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-AGE (err u400))

;; data vars
(define-data-var pet-counter uint u0)


;; data maps
;; Define pets variables
(define-map pets
    { owner: principal, pet-id: uint }
    {
        name: (string-utf8 100),
        breed: (string-utf8 100),
        age: uint,
        registration-date: uint
    }
)
;; public functions
;; Register a new pet
(define-public (register-pet (name (string-utf8 100)) (breed (string-utf8 100)) (age uint))
    (let
        (
            (new-id (get-next-pet-id))
            (owner tx-sender)
        )
        (asserts! (<= age u30) ERR-INVALID-AGE)
        (ok (map-set pets
            { owner: owner, pet-id: new-id }
            {
                name: name,
                breed: breed,
                age: age,
                registration-date: block-height
            }
        ))
    )
)

;; Update pet information
(define-public (update-pet-info (pet-id uint) (name (string-utf8 100)) (breed (string-utf8 100)) (age uint))
    (let
        (
            (owner tx-sender)
            (pet-data (map-get? pets { owner: owner, pet-id: pet-id }))
        )
        (asserts! (is-some pet-data) ERR-NOT-FOUND)
        (asserts! (<= age u30) ERR-INVALID-AGE)
        (ok (map-set pets
            { owner: owner, pet-id: pet-id }
            {
                name: name,
                breed: breed,
                age: age,
                registration-date: (get registration-date (unwrap! pet-data ERR-NOT-FOUND))
            }
        ))
    )
)

;; read only functions
;; Get pet information
(define-read-only (get-pet-info (owner principal) (pet-id uint))
    (match (map-get? pets { owner: owner, pet-id: pet-id })
        pet-data (ok pet-data)
        ERR-NOT-FOUND
    )
)

;; Get total number of pets for an owner
(define-read-only (get-owner-pet-count (owner principal))
    (var-get pet-counter)
)

;; Get pet by index for an owner
(define-read-only (get-owner-pet-by-id (owner principal) (pet-id uint))
    (map-get? pets { owner: owner, pet-id: pet-id })
)

;; Helper function to check if a pet exists
(define-read-only (pet-exists (owner principal) (pet-id uint))
    (is-some (map-get? pets { owner: owner, pet-id: pet-id }))
)

;; private functions
;; Get the next pet ID
(define-private (get-next-pet-id)
    (begin
        (var-set pet-counter (+ (var-get pet-counter) u1))
        (var-get pet-counter)
    )
)