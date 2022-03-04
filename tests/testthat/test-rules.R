Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "true")
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

test_that("`sv_optional()` basic use cases", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new(session)
    iv$add_rule("x", sv_optional())
    iv$add_rule("x", ~ "failure")
    shiny::isolate({
      expect_true(iv$is_valid())
      expect_identical(iv$validate(), rlang::list2(!!session$ns("x") := NULL))
    })

    session$setInputs(x = TRUE)
    shiny::isolate({
      expect_false(iv$is_valid())
      expect_identical(
        iv$validate(),
        rlang::list2(!!session$ns("x") := list(type = "error", message = "failure", is_html = FALSE))
      )
    })
  })
})

test_that("compose_rules short circuits", {
  got_here <- FALSE
  rule <- compose_rules(
    ~ "error",
    ~ { got_here <<- TRUE }
  )
  expect_identical(rule("check"), "error")
  expect_false(got_here)
})

test_that("compose_rules falls through", {
  got_here <- FALSE
  rule <- compose_rules(
    ~ { got_here <<- TRUE; NULL },
    ~ "error"
  )
  expect_identical(rule("check"), "error")
  expect_true(got_here)
})

test_that("compose_rules different input types", {
  input <- "hello"
  compose_rules(
    function(value) {
      expect_identical(value, input)
      NULL
    },
    ~ {
      expect_identical(., input)
      NULL
    }
  )(input)

  expect_error(compose_rules("not a function"))
})

expect_sv_fail <- function(rule, ...) {

  lapply(
    rlang::list2(...),
    FUN = function(x) {
      expect_type(rule(!!x), "character")
    }
  )
}

expect_sv_pass <- function(rule, ...) {

  lapply(
    rlang::list2(...),
    FUN = function(x) {
      expect_null(rule(!!x))
    }
  )
}

test_that("the `sv_lt()` rule function works properly", {

  always_pass <- list(-1, 0, 0L)
  always_fail <- list(1, 2, 2L, c(-1, 2), Inf)
  pass_if_multiple <- list(c(-10:-1))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(-2, -1, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf <- list(-Inf)

  rule <- sv_lt(rhs = 1)
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lt(rhs = 1, allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lt(rhs = 1, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lt(rhs = 1, allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lt(rhs = 1, allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan, !!!pass_if_inf)

  rule <- sv_lt(rhs = 1, allow_inf = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_inf)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na)
})

test_that("the `sv_lte()` rule function works properly", {

  always_pass <- list(-1, 0, 1, 1L)
  always_fail <- list(2, 2L, c(-1, 2), Inf)
  pass_if_multiple <- list(-10:1, c(-3, -5.2, 0.9))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(-2, -1, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf <- list(-Inf)

  rule <- sv_lte(rhs = 1)
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lte(rhs = 1, allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lte(rhs = 1, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lte(rhs = 1, allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_lte(rhs = 1, allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan, !!!pass_if_inf)

  rule <- sv_lte(rhs = 1, allow_inf = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_inf)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na)
})

test_that("the `sv_gte()` rule function works properly", {

  always_pass <- list(1, 2, 3, 3L)
  always_fail <- list(-2, -2L, -1, 0, c(-1, 2), -Inf)
  pass_if_multiple <- list(1:10, c(3, 5.2, 1.0))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(1, 2, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf <- list(Inf)

  rule <- sv_gte(rhs = 1)
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gte(rhs = 1, allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gte(rhs = 1, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gte(rhs = 1, allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gte(rhs = 1, allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan, !!!pass_if_inf)

  rule <- sv_gte(rhs = 1, allow_inf = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_inf)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na)
})

test_that("the `sv_gt()` rule function works properly", {

  always_pass <- list(2, 3, 3L)
  always_fail <- list(-2, -2L, -1, 0, c(-1, 2), -Inf)
  pass_if_multiple <- list(2:10, c(3, 5.2, 1.1))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(2, 3, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf <- list(Inf)

  rule <- sv_gt(rhs = 1)
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gt(rhs = 1, allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gt(rhs = 1, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gt(rhs = 1, allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na, !!!pass_if_inf)

  rule <- sv_gt(rhs = 1, allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan, !!!pass_if_inf)

  rule <- sv_gt(rhs = 1, allow_inf = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_inf)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na)
})

test_that("the `sv_equal()` rule function works properly", {

  usually_pass <- list(1, 1L, 1.0)
  usually_fail <- list(1.1, 0.9, -1L, 2L, c(-1, 2), -Inf, Inf)
  pass_if_multiple <- list(rep(1, 10))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(1, 1, NA), c(1, NA, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf_1 <- list(Inf)
  pass_if_inf_2 <- list(-Inf)

  rule <- sv_equal(rhs = 1)
  expect_sv_pass(rule, !!!usually_pass)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_equal(rhs = 1, allow_multiple = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_equal(rhs = 1, allow_na = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_equal(rhs = 1, allow_nan = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_equal(rhs = 1, allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_nan, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_equal(rhs = Inf, allow_inf = TRUE)
  expect_sv_pass(rule, !!!pass_if_inf_1)
  expect_sv_fail(rule, !!!pass_if_inf_2)

  rule <- sv_equal(rhs = Inf, allow_inf = TRUE)
  expect_sv_pass(rule, !!!pass_if_inf_1)
  expect_sv_fail(rule, !!!pass_if_inf_2)
})

test_that("the `sv_not_equal()` rule function works properly", {

  usually_pass <- list(2, 2L, 2)
  usually_fail <- list(1, 1L, -Inf, Inf)
  pass_if_multiple <- list(rep(2, 10))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(2, 2, NA), c(2, NA, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf_1 <- list(-Inf)
  pass_if_inf_2 <- list(Inf)

  rule <- sv_not_equal(rhs = 1)
  expect_sv_pass(rule, !!!usually_pass)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_not_equal(rhs = 1, allow_multiple = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_not_equal(rhs = 1, allow_na = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_1)

  rule <- sv_not_equal(rhs = 1, allow_nan = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_not_equal(rhs = 1, allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!usually_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!usually_fail, !!!pass_if_nan, !!!pass_if_inf_1, !!!pass_if_inf_2)

  rule <- sv_not_equal(rhs = Inf, allow_inf = TRUE)
  expect_sv_pass(rule, !!!pass_if_inf_1)
  expect_sv_fail(rule, !!!pass_if_inf_2)

  rule <- sv_not_equal(rhs = Inf, allow_inf = TRUE)
  expect_sv_pass(rule, !!!pass_if_inf_1)
  expect_sv_fail(rule, !!!pass_if_inf_2)
})

test_that("the `sv_between()` rule function works properly", {

  always_pass <- list(2, 3, 4L, 9, 3:5)
  always_fail <- list(0, -1L, c(-1, 5), Inf, -Inf)
  pass_if_na <- list(NA, NA_integer_, NA_real_, c(NA, NA, NA))
  pass_if_nan <- list(NaN, c(NaN, NaN))

  rule <- sv_between(1, 10)
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_nan)

  rule <- sv_between(1, 10, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan)

  rule <- sv_between(1, 10, allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na)

  expect_sv_fail(sv_between(1, 10, inclusive = c(FALSE, TRUE)), 1)
  expect_sv_fail(sv_between(1, 10, inclusive = c(TRUE, FALSE)), 10)
  expect_sv_fail(sv_between(1, 10, inclusive = c(FALSE, FALSE)), c(1, 10))
  expect_sv_fail(sv_between(1, 10, inclusive = c(FALSE, FALSE)), 1:2)
  expect_sv_fail(sv_between(1, 10, inclusive = c(FALSE, FALSE)), 9:10)
})

test_that("the `sv_in_set()` rule function works properly", {

  always_pass <- list(1, 2, 3, 4, 5, 5L)
  always_fail <-
    list(
      0, -1L, c(-5, 0), Inf, -Inf, NA, c(NA, NA, NA), c(4, NA), c(4, 6, 4), NaN,
      "", c("", "")
    )

  rule <- sv_in_set(1L:5L)
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail)

  rule <- sv_in_set(as.numeric(1:5))
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail)
})

test_that("the `sv_numeric()` rule function works properly", {

  always_pass <- list(-5E6, -1, 0, 0L, 5E6)
  always_fail <- list("text", TRUE, numeric(0), integer(0))
  pass_if_multiple <- list(c(-10:-1))
  pass_if_na <- list(NA_integer_, NA_real_)
  pass_if_nan <- list(NaN)
  pass_if_multiple_na <- list(c(-2, -1, NA), c(NA_real_, NA_real_), c(NA_integer_, 2))
  pass_if_inf <- list(-Inf, Inf)

  rule <- sv_numeric()
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <- sv_numeric(allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <- sv_numeric(allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <- sv_numeric(allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <- sv_numeric(allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan, !!!pass_if_inf)

  rule <- sv_numeric(allow_inf = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_inf)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na)
})

test_that("the `sv_integer()` rule function works properly", {

  always_pass <- list(-5L, -1234.0, 0L, 5E6L)
  always_fail <- list(5.6, c(NA_integer_, 2.0001), Inf, -Inf, integer(0), numeric(0))
  pass_if_multiple <- list(as.integer(-10:-1), as.numeric(3:6))
  pass_if_na <- list(NA_integer_)
  pass_if_nan <- list(NaN)
  pass_if_multiple_na <-
    list(
      c(-2L, -1L, NA_integer_), c(NA_integer_, NA_integer_),
      c(NA_integer_, 2L), c(NA_real_, 2.0), c(NA_real_, NA_real_),
      c(NA, NA, NA)
    )

  rule <- sv_integer()
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_integer(allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_integer(allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_multiple_na)

  rule <- sv_integer(allow_nan = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_integer(allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_nan)
})

test_that("the `sv_regex()` rule function works properly", {

  expect_null(
    sv_regex(
      pattern = "^T",
      message = "message"
    )("The shinyvalidate package.")
  )

  expect_equal(
    sv_regex(
      pattern = "^t",
      message = "message"
    )("The shinyvalidate package."),
    "message"
  )

  expect_null(
    sv_regex(
      pattern = "^t",
      message = "message",
      invert = TRUE
    )("The shinyvalidate package.")
  )

  expect_null(
    sv_regex(
      pattern = "^t",
      message = "message",
      ignore.case = TRUE
    )("The shinyvalidate package.")
  )

  expect_null(
    sv_regex(
      pattern = "{shinyvalidate}",
      message = "message",
      fixed = TRUE
    )("The {shinyvalidate} package.")
  )
})

test_that("the `sv_email()` rule function works properly", {

  # Examples taken from: https://github.com/manishsaraan/email-validator
  always_pass <-
    c(
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@letters-in-local.org",
      "01234567890@numbers-in-local.net",
      "&'*+-./=?^_{}~@other-valid-characters-in-local.net",
      "mixed-1234-in-{+^}-local@sld.net",
      "a@single-character-in-local.org",
      "one-character-third-level@a.example.com",
      "single-character-in-sld@x.org",
      "local@dash-in-sld.com",
      "letters-in-sld@123.com",
      "one-letter-sld@x.org",
      "test@test--1.com",
      "uncommon-tld@sld.museum",
      "uncommon-tld@sld.travel",
      "uncommon-tld@sld.mobi",
      "country-code-tld@sld.uk",
      "country-code-tld@sld.rw",
      "local@sld.newTLD",
      "the-total-length@of-an-entire-address.cannot-be-longer-than-two-hundred-and-fifty-four-characters.and-this-address-is-254-characters-exactly.so-it-should-be-valid.and-im-going-to-add-some-more-words-here.to-increase-the-lenght-blah-blah-blah-blah-bla.org",
      "the-character-limit@for-each-part.of-the-domain.is-sixty-three-characters.this-is-exactly-sixty-three-characters-so-it-is-valid-blah-blah.com",
      "local@sub.domains.com",
      "backticks`are`legit@test.com",
      "digit-only-domain@123.com",
      "digit-only-domain-with-subdomain@sub.123.com",
      "`a@a.fr",
      "`aa@fr.com",
      "com@sil.c1m",
      "t119037jskc_ihndkdoz@aakctgajathzffcsuqyjhgjuxnuulgnhxtnbquwtgxljfayeestsjdbalthtddy.lgtmsdhywswlameglunsaplsblljavswxrltovagexhtttodqedmicsekvpmpuu.pgjvdmvzyltpixvalfbktnnpjyjqswbfvtpbfsngqtmhgamhrbqqvyvlhqigggv.nxqglspfbwdhtfpibcrccvctmoxuxwlunghhwacjtrclgirrgppvshxvrzkoifl"
    )

  always_fail <-
    c(
      "@missing-local.org",
      "! #$%`|@invalid-characters-in-local.org",
      "(),:;`|@more-invalid-characters-in-local.org",
      "<>@[]\\`|@even-more-invalid-characters-in-local.org",
      # ".local-starts-with-dot@sld.com",
      # "local-ends-with-dot.@sld.com",
      # "two..consecutive-dots@sld.com",
      "partially.\"quoted\"@sld.com",
      # "the-local-part-is-invalid-if-it-is-longer-than-sixty-four-characters@sld.net",
      "missing-sld@.com",
      # "sld-starts-with-dashsh@-sld.com",
      # "sld-ends-with-dash@sld-.com",
      "invalid-characters-in-sld@! \"#$%(),/;<>_[]`|.org",
      "missing-dot-before-tld@com",
      "missing-tld@sld.",
      "invalid",
      # "the-total-length@of-an-entire-address.cannot-be-longer-than-two-hundred-and-fifty-six-characters.and-this-address-is-257-characters-exactly.so-it-should-be-invalid.and-im-going-to-add-some-more-words-here.to-increase-the-lenght-blah-blah-blah-blah-blah-.org",
      # "the-character-limit@for-each-part.of-the-domain.is-sixty-three-characters.this-is-exactly-sixty-four-characters-so-it-is-invalid-blah-blah.com",
      "missing-at-sign.net",
      "unbracketed-IP@127.0.0.1",
      # "invalid-ip@127.0.0.1.26",
      # "another-invalid-ip@127.0.0.256",
      "IP-and-port@127.0.0.1:25",
      "trailing-dots@test.de.",
      #"dot-on-dot-in-domainname@te..st.de",
      #"dot-first-in-domain@.test.de",
      "mg@ns.i",
      # ".dot-start-and-end.@sil.com",
      "double@a@com",
      ""
      # "tr119037jskc_ihndkdoz@d.aakctgajathzffcsuqyjhgjuxnuulgnhxtnbquwtgxljfayeestsjdbalthtddy.lgtmsdhywswlameglunsaplsblljavswxrltovagexhtttodqedmicsekvpmpuu.pgjvdmvzyltpixvalfbktnnpjyjqswbfvtpbfsngqtmhgamhrbqqvyvlhqigggv.nxqglspfbwdhtfpibcrccvctmoxuxwlunghhwacjtrclgirrgppvshxvrzkoifl"
    )

  pass_if_multiple <- list(c("single-character-in-sld@x.org", "local@dash-in-sld.com"))
  pass_if_na <- list(NA_integer_)
  pass_if_multiple_na <- list(c("single-character-in-sld@x.org", NA))

  rule <- sv_email()
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_email(allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_email(allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_multiple_na)

  rule <- sv_email(allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail)
})

test_that("the `sv_url()` rule function works properly", {

  always_pass <-
    c(
      "http://foo.com/blah_blah",
      "http://foo.com/blah_blah/",
      "http://foo.com/blah_blah_(wikipedia)",
      "http://\u2605foo.com/blah_blah_(wikipedia)_(again)",
      "http://www.example.com/wpstyle/?p=364",
      "https://www.example.com/foo/?bar=baz&inga=42&quux",
      "http://userid:password@example.com:8080",
      "http://userid:password@example.com:8080/",
      "http://userid@example.com",
      "http://userid@example.com/",
      "http://userid@example.com:8080",
      "http://userid@example.com:8080/",
      "http://userid:password@example.com",
      "http://userid:password@example.com/",
      "http://foo.com/blah_(wikipedia)#cite-1",
      "http://foo.com/blah_(wikipedia)_blah#cite-1",
      "http://foo.com/(something)?after=parens",
      "http://code.google.com/events/#&product=browser",
      "http://j.mp",
      "ftp://foo.bar/baz",
      "http://foo.bar/?q=Test%20URL-encoded%20stuff",
      "http://-.~_!$&'()*+,;=:%40:80%2f::::::@example.com",
      "http://1337.net",
      "http://a.b-c.de",
      "http://223.255.255.254"
    )

  always_fail <-
    c(
      "http://",
      "http://.",
      "http://..",
      "http://../",
      "http://?",
      "http://??",
      "http://??/",
      "http://#",
      "http://##",
      "http://##/",
      "http://foo.bar?q=Spaces should be encoded",
      "//",
      "//a",
      "///a",
      "///",
      "http:///a",
      "foo.com",
      "rdar://1234",
      "h://test",
      "http:// shouldfail.com",
      ":// should fail",
      "http://foo.bar/foo(bar)baz quux",
      "ftps://foo.bar/",
      "http://-error-.invalid/",
      "http://-a.b.co",
      "http://a.b-.co",
      "http://0.0.0.0",
      "http://3628126748",
      "http://.www.foo.bar/",
      "http://www.foo.bar./",
      "http://.www.foo.bar./"
    )

  pass_if_multiple <- list(c("http://foo.com/blah_blah", "http://foo.com/blah_blah/"))
  pass_if_na <- list(NA_integer_)
  pass_if_multiple_na <- list(c("http://foo.com/blah_blah", NA))

  rule <- sv_url()
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_url(allow_multiple = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_na, !!!pass_if_multiple_na)

  rule <- sv_url(allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!always_fail, !!!pass_if_multiple, !!!pass_if_multiple_na)

  rule <- sv_url(allow_multiple = TRUE, allow_na = TRUE)
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!always_fail)
})

test_that("the `sv_required()` rule function works properly", {

  always_pass <- list(3, "a", data.frame(a = 1:3))

  always_fail <- list("", rep("", 3), NULL, rep(NA, 3), character(0), numeric(0), logical(0))

  rule <- sv_required()
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!always_fail)
})

test_that("the `sv_basic()` rule function works properly", {

  always_pass <- list(-1, 0, 0L)
  pass_if_multiple <- list(c(-10:-1))
  pass_if_na <- list(NA, NA_integer_, NA_real_)
  pass_if_multiple_na <- list(c(-2, -1, NA), c(NA, NA, NA))
  pass_if_nan <- list(NaN)
  pass_if_inf <- list(-Inf)

  rule <-
    sv_basic(
      allow_multiple = FALSE,
      allow_na = FALSE,
      allow_nan = FALSE,
      allow_inf = FALSE
    )
  expect_sv_pass(rule, !!!always_pass)
  expect_sv_fail(rule, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <-
    sv_basic(
      allow_multiple = TRUE,
      allow_na = FALSE,
      allow_nan = FALSE,
      allow_inf = FALSE
    )
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple)
  expect_sv_fail(rule, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <-
    sv_basic(
      allow_multiple = FALSE,
      allow_na = TRUE,
      allow_nan = FALSE,
      allow_inf = FALSE
    )
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_na)
  expect_sv_fail(rule, !!!pass_if_multiple, !!!pass_if_nan, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <-
    sv_basic(
      allow_multiple = FALSE,
      allow_na = FALSE,
      allow_nan = TRUE,
      allow_inf = FALSE
    )
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_nan)
  expect_sv_fail(rule, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_inf, !!!pass_if_multiple_na)

  rule <-
    sv_basic(
      allow_multiple = TRUE,
      allow_na = TRUE,
      allow_nan = FALSE,
      allow_inf = FALSE
    )
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_multiple_na)
  expect_sv_fail(rule, !!!pass_if_nan, !!!pass_if_inf)

  rule <-
    sv_basic(
      allow_multiple = FALSE,
      allow_na = FALSE,
      allow_nan = FALSE,
      allow_inf = TRUE
    )
  expect_sv_pass(rule, !!!always_pass, !!!pass_if_inf)
  expect_sv_fail(rule, !!!pass_if_multiple, !!!pass_if_na, !!!pass_if_nan, !!!pass_if_multiple_na)
})

test_that("the `prepare_values_text()` prepares the text properly", {

  expect_values_text <- function(set, limit, expected) {
    expect_equal(prepare_values_text(set = set, limit = limit), expected = expected)
  }

  expect_values_text(set = 1:3, limit = 3, "1, 2, 3")
  expect_values_text(set = 1:2, limit = 3, "1, 2")
  expect_values_text(set = 1, limit = 3, "1")
  expect_values_text(set = 1:5, limit = 3, "1, 2, 3 (and 2 more)")
  expect_values_text(set = 1:5, limit = 2, "1, 2 (and 3 more)")
  expect_values_text(set = 1:5, limit = 10, "1, 2, 3, 4, 5")
  expect_values_text(set = 1:5, limit = Inf, "1, 2, 3, 4, 5")
})
