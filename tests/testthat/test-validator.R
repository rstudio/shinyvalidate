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
        !!session$ns("inputA") := "Input A is required",
        !!session$ns("inputB") := "Input B is required",
        !!session$ns("inputC") := "Input C is required"
      ))
    })
    
    session$setInputs(
      "inputB" = TRUE,
    )
    shiny::isolate({
      expect_false(iv$is_valid())
      expect_identical(iv$validate(), rlang::list2(
        !!session$ns("inputA") := "Input A is required",
        !!session$ns("inputC") := "Input C is required",
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
          !!session$ns("a") := "rule 1"
        ))
      }
      
      session$setInputs(a = TRUE)
      
      expect_identical(iv$validate(), rlang::list2(
        !!session$ns("a") := "rule 2"
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
