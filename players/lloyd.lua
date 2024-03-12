local m = {}

-- TODO: instead of a module with the 'meta' attribute, we could have a function
-- 'register' that would return the whole bot, that is, including event
-- handlers?
m.meta = { name = "Lloyd", color = { red = 250, green = 30, blue = 10 } }

function log(msg)
  print("[" .. m.meta.name .. "] " .. msg)
end

function m.on_tick(n)
  log("got tick event: " .. n)
  if n < 100 then
    log("heading: " .. me.heading())
    return { me.move(200), me.turn_right(0.02) }
  end
  p = me.position()
  log("got position: (" .. p.x .. ", " .. p.y .. ")")
end

-- function m.on_kill_enemy(enemy_name)
--    return { me.stop() }
-- end

log("reporting in")

return m
