-- Named queries.

-- name: get-fruits
SELECT name
FROM fruits

-- name: add-fruit!
-- Add a fruit.
--   fruit : the fruit name.
--   color : the fruit color.
INSERT INTO fruits (name, color)
VALUES (:fruit, :color)

-- name: update-orange!
-- Change the color of oranges.
UPDATE fruits
SET color = ?
WHERE name = 'orange'
