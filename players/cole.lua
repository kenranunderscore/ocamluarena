local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Cole", color = { red = 10, green = 130, blue = 170 } }

function log(msg)
  print("[" .. m.meta.name .. "] " .. msg)
end

log("reporting in")

return m
