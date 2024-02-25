local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 250, green = 30, blue = 10 } }

function log(msg)
  print("[" .. m.meta.name .. "] " .. msg)
end

log("reporting in")

function m.test()
  log("in test")
  log(me.x())
end

return m
