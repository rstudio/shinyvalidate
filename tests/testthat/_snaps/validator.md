# errors are correctly handled

    $`mock-session-a`
    $`mock-session-a`$type
    [1] "error"
    
    $`mock-session-a`$message
    [1] "An unexpected error occurred during input validation: normal error"
    
    
    $`mock-session-b`
    $`mock-session-b`$type
    [1] "error"
    
    $`mock-session-b`$message
    [1] "An unexpected error occurred during input validation: safe error"
    
    
    $`mock-session-c`
    $`mock-session-c`$type
    [1] "error"
    
    $`mock-session-c`$message
    [1] "An unexpected error occurred during input validation"
    
    

---

    $`mock-session-a`
    $`mock-session-a`$type
    [1] "error"
    
    $`mock-session-a`$message
    [1] "An unexpected error occurred during input validation"
    
    
    $`mock-session-b`
    $`mock-session-b`$type
    [1] "error"
    
    $`mock-session-b`$message
    [1] "An unexpected error occurred during input validation: safe error"
    
    
    $`mock-session-c`
    $`mock-session-c`$type
    [1] "error"
    
    $`mock-session-c`$message
    [1] "An unexpected error occurred during input validation"
    
    

