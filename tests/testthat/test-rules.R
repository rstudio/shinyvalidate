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
        rlang::list2(!!session$ns("x") := list(type = "error", message = "failure"))
      )
    })
  })
})
