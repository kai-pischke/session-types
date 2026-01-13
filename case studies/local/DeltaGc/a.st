b ? { 
  loop: rec t1. b ? { 
    loop: t1, 
    stop: rec t3. t3 
  }, 
  stop: rec t4. t4 
}
