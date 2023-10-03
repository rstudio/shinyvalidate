test_that("InputValidator can be constructed", {
  # Implicit session
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    expect_error(
      InputValidator$new(),
      NA
    )
  })

  # Explicit session
  expect_error(
    InputValidator$new(session = shiny::MockShinySession$new()),
    NA
  )
})

test_that("InputValidator cannot be constructed without a session", {
  expect_error(
    InputValidator$new(),
    "server function"
  )
})

test_that("InputValidator add_rule()", {
  if (packageVersion("shiny") <= "1.7.5" && getRversion() > "4.3.1") {
    skip("Skipping InputValidator add_rule() tests since shiny::need doesn't work as intended.")
  }
  
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new()

    iv$add_rule("inputA", shiny::need, message = "Input A is required")
    iv$add_rule("inputB", function(value) {
      if (is.null(value)) {
        "Input B is required"
      }
    })
    iv$add_rule("inputC", ~ if (is.null(.)) "Input C is required")

    expect_error(iv$is_valid(), "reactive context")

    shiny::isolate({
      expect_false(iv$is_valid())

      expect_identical(iv$validate(), rlang::list2(
        !!session$ns("inputA") := list(type = "error", message = "Input A is required", is_html = FALSE),
        !!session$ns("inputB") := list(type = "error", message = "Input B is required", is_html = FALSE),
        !!session$ns("inputC") := list(type = "error", message = "Input C is required", is_html = FALSE)
      ))
    })

    session$setInputs(
      "inputB" = TRUE,
    )
    shiny::isolate({
      expect_false(iv$is_valid())
      expect_identical(iv$validate(), rlang::list2(
        !!session$ns("inputA") := list(type = "error", message = "Input A is required", is_html = FALSE),
        !!session$ns("inputC") := list(type = "error", message = "Input C is required", is_html = FALSE),
        !!session$ns("inputB") := NULL
      ))
    })

    session$setInputs(
      "inputA" = TRUE,
      "inputC" = TRUE
    )
    shiny::isolate({
      expect_true(iv$is_valid())
      expect_identical(iv$validate(), rlang::list2(
        !!session$ns("inputA") := NULL,
        !!session$ns("inputB") := NULL,
        !!session$ns("inputC") := NULL
      ))
    })
  })
})

test_that("InputValidator add_rule() stops on first failing rule", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new()

    iv$add_rule("a", ~ if (!identical(., TRUE)) "rule 1")
    iv$add_rule("a", ~ if (!identical(., FALSE)) "rule 2")

    shiny::isolate({
      for (x in list(NULL, FALSE, "whatever")) {
        session$setInputs(a = x)
        expect_identical(iv$validate(), rlang::list2(
          !!session$ns("a") := list(type = "error", message = "rule 1", is_html = FALSE)
        ))
      }

      session$setInputs(a = TRUE)

      expect_identical(iv$validate(), rlang::list2(
        !!session$ns("a") := list(type = "error", message = "rule 2", is_html = FALSE)
      ))
    })
  })
})

test_that("Empty InputValidator works as expected", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new()
    shiny::isolate({
      expect_true(iv$is_valid())
      expect_identical(iv$validate(), list())
    })
  })
})

test_that("InputValidator$fields recurses over child validators", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    child_session <- session$makeScope(session$ns("child"))
    shiny::withReactiveDomain(child_session, {
      child_iv <- InputValidator$new()
      child_iv$add_rule("one", sv_required())
      child_iv$add_rule("one", sv_regex(".", "Must have a character"))
    })

    iv <- InputValidator$new()
    iv$add_validator(child_iv)
    # Same name but different namespace
    iv$add_rule("one", sv_required())

    shiny::isolate({
      expect_setequal(iv$fields(), names(iv$validate()))
      expect_length(iv$fields(), 2)

      expect_false(child_iv$is_valid())
      expect_false(iv$is_valid())

      # Let's also test conditionals while we're set up for it

      child_iv$condition(~ FALSE)
      expect_true(child_iv$is_valid())
      expect_false(iv$is_valid())

      iv$condition(~ FALSE)
      expect_true(child_iv$is_valid())
      expect_true(iv$is_valid())

      child_iv$condition(~ TRUE)
      expect_false(child_iv$is_valid())
      expect_true(iv$is_valid())
    })
  })
})

test_that("`skip_validation()` successfully skips remaining rules", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new(session)
    iv$add_rule("x", ~ skip_validation())
    iv$add_rule("x", ~ "failure")
    shiny::isolate({
      expect_true(iv$is_valid())
      expect_identical(iv$validate(), rlang::list2(!!session$ns("x") := NULL))
    })
  })
})

test_that("`skip_validation()` works with a child validator", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    child_iv <- InputValidator$new(session)
    child_iv$add_rule("x", ~ skip_validation())
    child_iv$add_rule("x", ~ "failure")
    shiny::isolate({
      expect_true(child_iv$is_valid())
    })

    iv <- InputValidator$new(session)
    iv$add_validator(child_iv)
    shiny::isolate({
      expect_true(iv$is_valid())
    })
    iv$add_rule("x", ~ "failure")
    shiny::isolate({
      expect_false(iv$is_valid())
    })
  })
})

test_that("only valid rule objects are accepted", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new(session)
    iv$add_rule("x", function(x) NULL)
    iv$add_rule("x", ~ NULL)
    expect_error(iv$add_rule("x", NULL))
    expect_error(iv$add_rule("x", "string"))
    expect_error(iv$add_rule("x", TRUE))
  })
})

test_that("errors are correctly handled", {
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    iv <- InputValidator$new(session)
    iv$add_rule("a", ~ stop("normal error"))
    iv$add_rule("b", ~ stop(shiny::safeError("safe error")))
    iv$add_rule("c", ~ shiny::req(FALSE))
    shiny::isolate({
      suppressWarnings(expect_warning(res <- iv$validate()))
      expect_snapshot_output(res)

      op <- options(shiny.sanitize.errors = TRUE)
      on.exit(options(op), add = TRUE)
      suppressWarnings(expect_warning(res <- iv$validate()))
      expect_snapshot_output(res)
    })
  })
})
