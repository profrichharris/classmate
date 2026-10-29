# ---------------------------------------------------------------------------
# PIN-protected key files
#
# A key file made by klausmate_make_key() is a "sealed" list: the real contents (API key, limits,
# expiry...) are encrypted with AES-256-CBC under a key derived from a 6-digit PIN with a slow
# key-derivation function (bcrypt_pbkdf), and authenticated with an HMAC so that a wrong PIN is
# detected cleanly. The PIN is not stored. Only `key_id` and `created` stay readable, so that a
# key that is already active on this computer is recognised without asking for the PIN again.
#
# This is a deterrent against casual misuse of a leaked key file, not strong protection: there are
# only a million PINs. The real safeguard is revoking the API key at Anthropic.
# ---------------------------------------------------------------------------

KLAUSMATE_PIN_MAX_TRIES <- 10L
KLAUSMATE_KEY_ROUNDS    <- 32L

klausmate_pin_ok <- function(pin)
  is.character(pin) && length(pin) == 1L && grepl("^[0-9]{6}$", pin)

.klausmate_derive <- function(pin, salt, rounds) {
  k <- openssl::bcrypt_pbkdf(charToRaw(pin), salt, as.integer(rounds), 64L)
  list(enc = k[1:32], mac = k[33:64])
}

# "sealed" (current format), "legacy" (a plain key file from before PINs) or "invalid"
# Key files made before the rename (with a classmate_key field) still open
klausmate_key_kind <- function(x) {
  if (is.list(x) && (identical(x$klausmate_key, 2L) || identical(x$classmate_key, 2L))) return("sealed")
  if (is.list(x) && is.character(x$api_key) && nzchar(x$api_key[1])) return("legacy")
  "invalid"
}

klausmate_seal_key <- function(payload, pin) {
  stopifnot(klausmate_pin_ok(pin))
  salt <- openssl::rand_bytes(16)
  iv   <- openssl::rand_bytes(16)
  k    <- .klausmate_derive(pin, salt, KLAUSMATE_KEY_ROUNDS)
  cipher <- openssl::aes_cbc_encrypt(serialize(payload, NULL), k$enc, iv)
  mac    <- as.raw(openssl::sha256(c(salt, iv, as.raw(cipher)), key = k$mac))
  list(klausmate_key = 2L, key_id = payload$key_id, created = payload$created,
       rounds = KLAUSMATE_KEY_ROUNDS, salt = salt, iv = iv, cipher = as.raw(cipher), mac = mac)
}

# The decrypted payload, or NULL when the PIN is wrong (or the file has been altered)
klausmate_open_key <- function(container, pin) {
  if (!klausmate_pin_ok(pin) || !identical(klausmate_key_kind(container), "sealed")) return(NULL)
  tryCatch({
    k   <- .klausmate_derive(pin, container$salt, container$rounds)
    mac <- as.raw(openssl::sha256(c(container$salt, container$iv, container$cipher), key = k$mac))
    if (!identical(mac, container$mac)) return(NULL)
    unserialize(openssl::aes_cbc_decrypt(container$cipher, k$enc, container$iv))
  }, error = function(e) NULL)
}

# Wrong-PIN counter: kept in an R option, so it lasts until R is restarted.
klausmate_pin_failures <- function() getOption("klausmate.pin_failures", 0L)
klausmate_pin_locked   <- function() klausmate_pin_failures() >= KLAUSMATE_PIN_MAX_TRIES
klausmate_pin_fail     <- function() options(klausmate.pin_failures = klausmate_pin_failures() + 1L)
klausmate_pin_tries_left <- function() max(0L, KLAUSMATE_PIN_MAX_TRIES - klausmate_pin_failures())
KLAUSMATE_PIN_LOCKED_MSG <- paste("Too many incorrect PINs. For security, Klausmate will not accept",
                                  "another attempt until R has been restarted.")

# Console version, for whisper(): read a key file, asking for its PIN if it is sealed.
# `current_cfg` is the key already active on this computer, if any; the same key is not asked
# for again. Returns the payload, or NULL (with a message) if it could not be unlocked.
klausmate_unlock_key_file <- function(path, current_cfg = NULL) {
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  switch(klausmate_key_kind(x),
    invalid = { message("That file does not appear to be a valid klausmate key."); NULL },
    legacy  = { message("That key file is from an older version of Klausmate and can no longer ",
                        "be used. Please ask your instructor for a new one."); NULL },
    sealed  = {
      if (!is.null(current_cfg) && !is.null(x$key_id) && identical(current_cfg$key_id, x$key_id))
        return(current_cfg)
      repeat {
        if (klausmate_pin_locked()) { message(KLAUSMATE_PIN_LOCKED_MSG); return(NULL) }
        pin <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
          rstudioapi::askForPassword("Enter the 6-digit PIN for this key")
        else readline("Enter the 6-digit PIN for this key (press Enter to cancel): ")
        pin <- trimws(pin %||% "")
        if (!nzchar(pin)) { message("Cancelled."); return(NULL) }
        payload <- klausmate_open_key(x, pin)
        if (!is.null(payload)) return(payload)
        klausmate_pin_fail()
        message("That PIN is not correct. ", klausmate_pin_tries_left(), " attempt(s) left.")
      }
    })
}
